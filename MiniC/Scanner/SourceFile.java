package MiniC.Scanner;

public class SourceFile {

  java.io.File source_file;
  java.io.FileInputStream source;
  final char EOL = '\n';
  final char EOT = '\u0000';

  public SourceFile (String filename) {
    try {
       source_file = new java.io.File(filename);
       source = new java.io.FileInputStream(source_file);
    } catch (java.io.IOException e) {
       source_file = null;
       source = null;
    }
  }

  public char readChar()
  {
     try {
        int c = source.read();
        if (c == -1) {
           c = EOT;
        }
        return (char) c;
     } catch (java.io.IOException e) {
        return EOT;
     }
  }

}
