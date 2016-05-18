package net.tqft.fix

import scala.util.Try
import scala.sys.process._
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import org.apache.commons.io.FilenameUtils
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object RenderBoard {
  val outputPath = Paths.get("/Users/scott/projects/fix/boards/")

  private def getProgramPath(programName: String, searchDirs: Seq[String]) = {
    val paths = for (
      dir <- searchDirs;
      file = new java.io.File(dir + s"/$programName");
      if file.exists
    ) yield file.toPath.toString

    paths.headOption.orElse(Try(s"which $programName".!!).toOption).getOrElse(programName)
  }

  private val texSearchPaths = List("/usr/texbin", "/Library/TeX/texbin", "/usr/bin", "/usr/local/bin")
  var pdflatexPath = getProgramPath("pdflatex", texSearchPaths)
  var pdftexPath = getProgramPath("pdftex", texSearchPaths)
  var gsPath = getProgramPath("gs", texSearchPaths)
  var pdfcropPath = getProgramPath("pdfcrop", texSearchPaths)

  def tikzString(board: Board): String = {
    board.coordinates().map(c => s"\\node at (${c(0)*0.6}cm,${c(1)*0.6}cm) {\\includegraphics{../pieces/${(board.states(board.index(c)).toString.replaceAllLiterally(",","") + "??").take(2)}}};").mkString("\\begin{tikzpicture}\n","\n","\\end{tikzpicture}\n")
  }

  def writeBoardPDF(g: Board)(filename: String = filenameForBoard(g)): Path = {
    val path = outputPath.resolve(outputPath.resolve(filename))
    pdf(path, Seq(g))
    path
  }
  
  def writePuzzlePDF(g: Puzzle)(filename: String = filenameForPuzzle(g)): Path = {
    val path = outputPath.resolve(outputPath.resolve(filename))
    pdf(path, g.puzzlePieces)
    path
  }

  private def SHA1(message: String) = {
    val md: MessageDigest = MessageDigest.getInstance("SHA-1")
    val bytes = message.getBytes("CP1252")

    BigInt(1,md.digest(bytes)).toString(16)
  }
  
  private def filenameForBoard(g: Board) = SHA1(g.toString) + ".pdf"
  private def filenameForPuzzle(g: Puzzle) = g.board.dimensions.mkString("x") + "-" + SHA1(g.toString) + ".pdf"

  def createBoardPDF(g: Board) = {
    val path = outputPath.resolve(filenameForBoard(g))
    if (Files.exists(path)) {
      path
    } else {
      writeBoardPDF(g)()
    }
  }
  def createPuzzlePDF(g: Puzzle) = {
    val path = outputPath.resolve(filenameForPuzzle(g))
    if (Files.exists(path)) {
      path
    } else {
      writePuzzlePDF(g)()
    }
  }

  def pdf(pdfPath: Path, boards: Seq[Board]) = {

    // Writes TikZ to tex file and runs pdflatex
    val outputStr = boards.map(tikzString).mkString(
      "\\documentclass{article}\n\\usepackage{tikz}\n\\pagestyle{empty}\n\\begin{document}\n",
      "",
      "\n\\end{document}")

      
      
    val baseName = FilenameUtils.getBaseName(pdfPath.toString)
    val fullPath = FilenameUtils.getFullPath(pdfPath.toString)
    Files.write(Paths.get(s"$fullPath$baseName.tex"), (outputStr).getBytes(StandardCharsets.UTF_8))
    val pdflatexCommand = s"$pdflatexPath ${if (fullPath == "") "" else s"-output-directory=$fullPath"} $fullPath$baseName.tex"
    val pdfcropCommand = s"$pdfcropPath $fullPath$baseName.pdf --gscmd $gsPath --pdftexcmd $pdftexPath"
    val gsCommand = s"$gsPath -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$fullPath$baseName.pdf $fullPath$baseName-crop.pdf"

    try Process(pdflatexCommand, outputPath.toFile).!!
    catch {
      case e: java.lang.Exception => { println(s"Error: Problem running '$pdflatexCommand'! Maybe check the filename and that the path exists?"); throw e }
    }
    try pdfcropCommand.!!
    catch {
      case e: java.lang.Exception => { println(s"Error: Problem running '$pdfcropCommand'! Do you have pdfcrop installed?"); throw e }
    }
    try gsCommand.!!
    catch {
      case e: java.lang.Exception => { println(s"Error: Problem running '$gsCommand'!"); throw e }
    }
    // Clean up
    for (ext <- List(".aux", ".log", "-crop.pdf")) {
      Files.deleteIfExists(outputPath.resolve(s"$baseName$ext"))
    }
  }

}