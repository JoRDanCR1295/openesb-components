/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)Compiler.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.compilers;

import java.io.*;

abstract public class Compiler 
{
  protected String indent = "";
  protected static final String TAB = "  ";
  protected static final String TABTAB = TAB+TAB;
  protected static final String TABTABTAB = TABTAB+TAB;
  protected static final String TABTABTABTAB = TABTABTAB+TAB;
  protected static final String TABTABTABTABTAB = TABTABTABTAB+TAB;
  protected String rootDir = ".";
  protected String rootNodeName;
  protected PrintStream currStream;
  static public String lineSep = System.getProperty("line.separator");
  
  public Compiler()
  {
  }

  public void setRootDirectory(String s)
  {
    rootDir = s;
  }

  public void setRootNodeName(String s)
  {
    rootNodeName = s;
  }

  public String getRootNodeName()
  {
    return rootNodeName;
  }

  public String getRootDirectory()
  {
    return rootDir;
  }

  public static PrintStream createPrintStream(String s)
    throws FileNotFoundException, IOException
  {
    File f = new File(s);
    String directory = f.getParent();
    String fname = f.getName();
    if (null == directory)
    {
      directory = ".";
    }
    return Compiler.createPrintStream(directory, fname);
  }

  public static PrintStream createPrintStream(String directory, String fname)
    throws FileNotFoundException, IOException
  {
    File rt= new File(directory);
    if(!rt.exists()) 
    {
      rt.mkdirs();
    }
    File fout = new File(rt,fname);
    PrintStream pstream = new PrintStream(new BufferedOutputStream(new FileOutputStream(fout)));
    return pstream;
    
  }

  public void setOut(PrintStream pstream) 
  {
    currStream = pstream;
  }

    public PrintStream getOut() {
	return currStream;
    }

  public void emit(String indent,String s)
  {
    //        System.out.println(indent+s);
    if (currStream != null) 
    {
      currStream.println(indent+s);
    }
  }
  // LT added
  public void emit(StringBuffer sb, String indent, String s)
  {
    sb.append(indent).append(s).append("\n");
  }
  // LT added
  public void emit(PrintStream ps, String indent, String s)
  {
    ps.println(indent+s);
  }
  // LT added
  public void emit(StringBuffer sb, String indent, StringBuffer content)
  {
    sb.append(indent).append(content).append("\n");
  }
  // LT added
  public void emit(PrintStream ps, String indent, StringBuffer content)
  {
    ps.println(content.insert(0, indent));
  }
  // LT added: for retrograde addition into StringBuffer
  public void rmit(StringBuffer sb, String indent, String s)
  {
    sb.insert(0, indent+s+"\n");
  }
  // LT added utility function
  public String isNullLevel(String level, String defaultLevel)
  {
    if (level!=null && level.length() != 0)
      return "(short)"+level;
    else
      return defaultLevel;
  }

  public static BufferedReader getFileReader(String fname)
    throws IOException
  {
    File f = new File(fname);
    return getFileReader(f);
  }

  public static BufferedReader getFileReader(File f)
    throws IOException
  {
    return new BufferedReader(new FileReader(f));
  }
}
