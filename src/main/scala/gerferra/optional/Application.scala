package gerferra
package optional

import scala.reflect.ClassTag
import scala.reflect.runtime.{ universe => ru }
import scala.annotation.tailrec
import scala.util.{ Try, Failure, Success }

import Application._

trait Application extends Logging {

  def main(args: Array[String]) {
    runMain(this, args.toList)
  }
  

  private def runMain[T: ClassTag](instance: T, args: List[String]) {

    val instanceMirror = ru.runtimeMirror(getClass.getClassLoader).reflect(instance)
    val theType = instanceMirror.symbol.typeSignature

    val methods =
      theType.members.collect {
        case m if m.isMethod => m.asMethod
      }

    val mainMethods = methods.filter(m => m.name == ru.TermName("main") && m.fullName != classOf[Application].getName + ".main")

    log.debug(s"mainMethods: ${mainMethods.map(_.fullName)}")

    val methodOrError = mainMethods.headOption.toRight("No se econtró un método main")
    val paramsOrError = methodOrError.right.map(m => m.paramLists.headOption.getOrElse(Nil)) // me importa solo la primer lista de parámetros (si es que existe una)

    val invocationOrError =
      for {
        method <- methodOrError.right
        params <- paramsOrError.right
        values <- instantiate(params, args).right
      } yield {
        log.debug(s"method: $method")
        log.debug(s"values: $values")
        log.debug(s"values: ${values.unzip._2}")
        val refMethod = instanceMirror.reflectMethod(method)
        log.debug(s"refMethod: $refMethod")
        refMethod(values.unzip._2: _*)
      }

    invocationOrError.fold(println, identity /* no hago nada porque ya ejecutó como efecto secundario */ )

  }

}

object Application extends Logging {

  type Error = String
  type Name = String
  type StringValue = String

  val Param = """--(\w*)""".r

  /**
   * Procesa la lista de argumentos identificando los que estás definidos con nombre
   */
  def assignNames(args: List[String]): List[(Option[String], String)] = {

    @tailrec
    def assignNames(args: List[String], result: List[(Option[Name], StringValue)]): List[(Option[Name], StringValue)] = args match {
      case Param(paramName) :: (rest @ (Param(_) :: _)) =>
        // un parámetro sin valor se asume que es un booleano
        assignNames(rest, Some(paramName) -> "true" :: result)
        
      case Param(paramName) :: Nil =>
        // un parámetro sin valor se asume que es un booleano
        (Some(paramName) -> "true" :: result).reverse
        
      case Param(paramName) :: paramValue :: rest =>
        assignNames(rest, Some(paramName) -> paramValue :: result)

      case paramValue :: rest =>
        assignNames(rest, None -> paramValue :: result)

      case Nil =>
        result.reverse
    }

    assignNames(args, Nil)
  }

  /**
   * Devuelve o un cast del `valueStr` al `paramType` o un `Error`
   */
  def coerce(paramName: Name, valueStr: StringValue, paramType: ru.Type): Either[Error, Any] = {

    val defError = s"Error interpretando parámetro `$paramName`: `$valueStr` no es un `$paramType` válido"

    if (paramType =:= ru.typeOf[String]) {

      Right(valueStr)

    } else if (paramType =:= ru.typeOf[Int]) {

      Try(valueStr.toInt).fold(Right(_), _ => Left(defError))

    } else if (paramType =:= ru.typeOf[Boolean]) {

      Try(valueStr.toBoolean).fold(Right(_), _ => Left(defError))

    } else if (paramType =:= ru.typeOf[java.io.File]) {
      
      Try(new java.io.File(valueStr)).fold(Right(_), _ => Left(defError))
      
    } else if (paramType <:< ru.typeOf[Option[_]]) {
      val ru.TypeRef(_, _, innerType :: Nil) = paramType

      coerce(paramName, valueStr, innerType).right.map(any => Some(any))

    } else {

      Left(s"Error interpretando parámetro `$paramName`: no se como covertir un argumento al tipo `$paramType`")

    }
  }

  def instantiateUnreferencedParams(unreferencedParams: List[(Name, ru.Type)], unnamedArgs: List[StringValue]): Either[Error, List[(Name, Any)]] = {

    @tailrec
    def instantiateUnreferencedParams(unreferencedParams: List[(Name, ru.Type)], unnamedArgs: List[StringValue], resultOrError: Either[Error, List[(Name, Any)]]): Either[Error, List[(Name, Any)]] = {

      def prepend(name: Name, valueOrError: Either[Error, Any]): Either[Error, List[(Name, Any)]] = {
        val newResultOrError =
          for {
            result <- resultOrError.right
            value <- valueOrError.right
          } yield {
            name -> value :: result
          }

        newResultOrError
      }
      
      (unreferencedParams, unnamedArgs) match {

        case (p :: prest, a :: arest) =>
          /* caso general */

          val valueOrError = coerce(p._1, a, p._2)

          val newResultOrError = prepend(p._1, valueOrError)

          instantiateUnreferencedParams(prest, arest, newResultOrError)

        case (Nil, Nil) =>
          /* caso final OK */

          resultOrError

        case (Nil, a :: rest) =>
          /* quedan valores para procesar pero no quedan más parámetros que no hayan sido definidos por nombre */
          // XXX a este caso podría buscársele la vuelta para manejar varargs, ie, asumir los parámetros restantes como una lista que va al final ...
          Left(s"No se a que parámetros asociar los siguientes valores: ${unnamedArgs.mkString(", ")}")

        case (p :: prest, Nil) =>
          /* me quedan parámetros para asignar pero no tengo más valores */

          if (p._2 <:< ru.typeOf[Option[_]]) {
           /* si no hay más valores pero los parámetros que quedan son `Option[_]`, 
            * asumo que se utiliza el valor `None` 
            */

            val newResultOrError = prepend(p._1, Right(None))

            instantiateUnreferencedParams(prest, Nil, newResultOrError)

          } else {
            Left(s"No se han especificado los parámetros: ${unreferencedParams.unzip._1.mkString(", ")}")
          }
      }
      
    }

    instantiateUnreferencedParams(unreferencedParams, unnamedArgs, Right(Nil))
  }

  def instantiateParams(params: List[(Name, ru.Type)], args: List[(Name, StringValue)]): Either[Error, List[(Name, Any)]] = {

    @tailrec
    def instantiateParams(params: List[(Name, ru.Type)], args: List[(Name, StringValue)], resultOrError: Either[Error, List[(Name, Any)]]): Either[Error, List[(Name, Any)]] =
      (params, args) match {

        case ((paramName, paramType) :: prest, (argName, argValue) :: arest) if paramName == argName =>
          /* caso general */

          val valueOrError = coerce(paramName, argValue, paramType)

          val newResultOrError =
            for {
              result <- resultOrError.right
              value <- valueOrError.right
            } yield {
              paramName -> value :: result
            }

          instantiateParams(prest, arest, newResultOrError)

        case (Nil, Nil) =>
          /* caso final OK */

          resultOrError

        case (Nil, a :: rest) =>
          /* quedan valores para procesar pero no quedan más parámetros que no hayan sido definidos por nombre */

          Left(s"No se a qué parámetros asociar los siguientes valores: ${args.mkString(", ")}")

        case ((paramName, paramType) :: prest, (argName, argValue) :: arest) if paramName != argName =>
          /* saltea parámetro paramName */

          if (paramType <:< ru.typeOf[Option[_]]) {
            /* si el parámetro que se saltea es `Option[_]`, asumo que se utiliza el valor `None` */

            val newResultOrError =
              for {
                result <- resultOrError.right
              } yield {
                paramName -> None :: result
              }

            instantiateParams(prest, arest, newResultOrError)

          } else {

            Left(s"No se ha especificado un valor para el parámetro `$paramName`")

          }

        case ((paramName, paramType) :: prest, Nil) =>
          /* me quedan parámetros para asignar pero no tengo más valores */

          if (paramType <:< ru.typeOf[Option[_]]) {
            /* si no hay más valores pero los parámetros que quedan son `Option[_]`, 
           * asumo que se utiliza el valor `None` 
           */

            val newResultOrError =
              for {
                result <- resultOrError.right
              } yield {
                paramName -> None :: result
              }

            instantiateParams(prest, Nil, newResultOrError)

          } else {

            Left(s"No se ha especificado un valor para el parámetro `$paramName`")

          }
      }

    instantiateParams(params, args, Right(Nil))
  }

  def instantiate(paramList: List[ru.Symbol], argList: List[String]): Either[Error, List[(Name, Any)]] = {

    val params = paramList.map(p => p.name.encodedName.toString -> p.typeSignature)
    val paramNames = params.unzip._1

    log.debug(s"params: $params")

    val argsOptName = assignNames(argList)

    log.debug(s"argsOptName: $argsOptName")

    val argsPartition = argsOptName.partition(_._1.isEmpty)

    log.debug(s"argsPartition: $argsPartition")

    val unnamedArgs = argsPartition._1.unzip._2
    val namedArgsTmp =
      argsPartition._2.collect {
        case (Some(n), value) => n -> value
      }

    log.debug(s"argsPartition2: ${(unnamedArgs -> namedArgsTmp)}")

    def sortFunc(s: String): Int = {
      val i = paramNames.indexOf(s)
      if (i == -1) Int.MaxValue else i
    }

    val namedArgs = namedArgsTmp.sortBy(p => sortFunc(p._1))

    log.debug(s"argsPartition sorted: ${(unnamedArgs -> namedArgs)}")

    val argsNames = namedArgs.unzip._1
    val (unreferencedParams, referencedParams) = params.partition(p => !argsNames.contains(p._1))

    log.debug(s"unreferencedParams, referencedParams: ${(unreferencedParams, referencedParams)}")

    val unreferenced = instantiateUnreferencedParams(unreferencedParams, unnamedArgs)

    log.debug(s"unreferenced: $unreferenced")

    val referenced = instantiateParams(referencedParams, namedArgs)

    log.debug(s"referenced: $referenced")

    val res =
      for {
        unref <- unreferenced.right
        ref <- referenced.right
      } yield {
        (unref ++ ref).sortBy(p => sortFunc(p._1))
      }

    log.debug(s"res: $res")

    res
  }

}
