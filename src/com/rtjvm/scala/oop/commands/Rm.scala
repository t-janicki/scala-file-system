package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Rm(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    val absolutePath =
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name

    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Nuclear war not supported yet!")
    else
      doRm(state, absolutePath)
  }

  def doRm(state: State, path: String): State = {
    def rmHelper(currentDir: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDir
      else if (path.tail.isEmpty) currentDir.removeEntry(path.head)
      else {
        val nextDir = currentDir.findEntry(path.head)
        if (!nextDir.isDirectory) currentDir
        else {
          val newNextDir = rmHelper(nextDir.asDirectory, path.tail)
          if (newNextDir == nextDir) currentDir
          else currentDir.replaceEntry(path.head, newNextDir)
        }
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if (newRoot == state.root)
      state.setMessage(path + ": no such file or directory")
    else
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }
}
