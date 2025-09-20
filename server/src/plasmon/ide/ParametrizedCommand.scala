package plasmon.ide

import plasmon.ide.JsonParser._
import scala.jdk.CollectionConverters.*

import org.eclipse.{lsp4j => l}

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/Command.scala#L63-L101
case class ParametrizedCommand[T](
  id: String,
  title: String
) {
  def toLsp(argument: T): l.Command =
    new l.Command(title, id, List(argument.toJson.asInstanceOf[AnyRef]).asJava)

  def toExecuteCommandParams(argument: T): l.ExecuteCommandParams =
    new l.ExecuteCommandParams(
      id,
      List[Object](
        argument.toJson
      ).asJava
    )
}
