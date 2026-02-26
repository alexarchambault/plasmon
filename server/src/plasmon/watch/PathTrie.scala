// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/watcher/PathTrie.scala

package plasmon.watch

import scala.jdk.CollectionConverters.*

/** Trie representation of a set of paths
  *
  * Each path segment is represented by a node in a tree. Can be used to efficiently check if the
  * trie contains a prefix of a given path
  */
private class PathTrie private (root: PathTrie.Node) {

  import PathTrie.*

  def containsPrefixOf(path: os.Path): Boolean = {
    def go(segments: List[String], node: Node): Boolean =
      (segments, node) match {
        case (_, Leaf) => true
        case (Nil, _)  => false
        case (head :: tail, Single(segment, child, _)) =>
          if (head == segment) go(tail, child) else false
        case (head :: tail, Multi(children, _)) =>
          children.get(head).fold(false)(go(tail, _))
      }
    go(path.segments.toList, root)
  }

  def longestPrefixes(fsRoot: os.Path, maxRoots: Int): Iterable[os.Path] = {
    def go(acc: os.Path, node: Node, availableRoots: Int): Iterable[os.Path] =
      node match {
        case Leaf => acc :: Nil
        case Single(segment, child, terminal) =>
          if (terminal) acc :: Nil
          else go(acc / segment, child, availableRoots)
        case Multi(children, terminal) =>
          if (terminal || children.size > availableRoots) acc :: Nil
          else
            children.flatMap {
              case (segment, child) =>
                go(acc / segment, child, availableRoots / children.size)
            }
      }
    go(fsRoot, root, maxRoots)
  }
}

private object PathTrie {
  private sealed trait Node

  private case object Leaf extends Node
  private case class Single(segment: String, child: Node, terminal: Boolean)
      extends Node
  private case class Multi(children: Map[String, Node], terminal: Boolean)
      extends Node

  def apply(paths: Set[os.Path], workspace: os.Path): PathTrie = {
    def construct(paths: Set[List[String]]): Node = {
      val terminal = paths.contains(Nil)
      val groupedNonEmptyPaths = paths
        .filter(_.nonEmpty)
        .groupBy(_.head)
        .toList
        .map {
          case (k, v) =>
            (k, v.map(_.tail))
        }

      groupedNonEmptyPaths match {
        case Nil => Leaf
        case singleGroup :: Nil =>
          Single(singleGroup._1, construct(singleGroup._2), terminal)
        case _ =>
          val children = groupedNonEmptyPaths.map {
            case (topSegment, tailSegments) =>
              topSegment -> construct(tailSegments)
          }.toMap
          Multi(children, terminal)
      }
    }

    /** NOTE(jkciesluk): If we don't have any paths, PathTrie would represent the entire file
      * system. PathTrie was introduced as a optimization to not watch the whole workspace, so when
      * the paths are empty we should just watch the workspace.
      */
    if (paths.isEmpty)
      new PathTrie(
        construct(
          Set(workspace.segments.toList)
        )
      )
    else
      new PathTrie(
        construct(
          paths.map(_.segments.toList)
        )
      )
  }
}
