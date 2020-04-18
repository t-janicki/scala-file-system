package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {

  def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$name must not contain separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(s"$name: illegal entry name!")
    } else {
      doCreateEntry(state, name)
    }
  }

  def doCreateEntry(state: State, entryName: String): State = {
    def updatesStructure(currentDir: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {

        val oldEntry = currentDir.findEntry(path.head).asDirectory
        currentDir.replaceEntry(oldEntry.name, updatesStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd

    val allDirsInPath = wd.getAllFoldersInPath

    val newEntry = createSpecificEntry(state)

    val newRoot = updatesStructure(state.root, allDirsInPath, newEntry)

    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def checkIllegal(str: String): Boolean = {
    name.contains(".")
  }

  def createSpecificEntry(state: State): DirEntry
}
