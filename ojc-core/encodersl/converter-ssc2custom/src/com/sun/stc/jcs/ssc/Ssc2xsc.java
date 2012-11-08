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
 * @(#)Ssc2xsc.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import org.xml.sax.SAXException;

import com.sun.stc.jcsre.Option;

/**
 * Class for the conversion of SSC to XSC event descriptions.
 * Used to use the Monk engine (through "stctrans") for parsing SSC
 * up through e*Gate 4.5.1, but now just gates to SscConv.
 */
public class Ssc2xsc
{
    /**
     * Converts input SSC files to out XSC files.  The package name is noted
     * in the XSC file.  Return non-zero status if conversion failed.
     * Usage: main [-option arg] file.ssc ...
     *
     * @param args  the command-line arguments
     */
    public static void main (String[] args)
    {
	File idir = new File(".");
	File odir = new File(".");
	Vector loadv = new Vector();
	String prepath = "";
	String prepack = "";
	String dumb = null;
	File tmpMonk = null;
	File tmpXsc = null;
	StringBuffer sx = new StringBuffer();
	HashSet ext = new HashSet();
	boolean uconv = false;
	boolean debug = false;
	int c = 0;

	Option opt = new Option(args, "de:i:o:jl:a:p:um:x:");
	opt.debug = true;
	while ((c = opt.getOpt()) != Option.EOF)
	{
	    switch (c)
	    {
	    case 'd':
		// Switch on debugging.
		debug = true;
		break;
	    case 'e':
		// Dumb conversion for extern template refs.
		dumb = opt.getOptArg();
		break;
	    case 'i':
		// Input root directory.
		idir = new File(opt.getOptArg());
		break;
	    case 'j':
		// Ignore; used to be "pure Java" switch.
		break;
	    case 'o':
		// Ouput root directory.
		odir = new File(opt.getOptArg());
		break;
	    case 'l':
		// Load-path element, relative to input root; may be empty
		// (meaning input root itself).
		loadv.add(opt.getOptArg());
		break;
	    case 'a':
		// Assumed parent in XSC tree.
		prepath = opt.getOptArg();
		break;
	    case 'p':
		// Package prefix.
		prepack = opt.getOptArg();
		break;
	    case 'u':
		// Convert non-ASCII to U-escapes in Java names.
		// (only works with -j).
		uconv = true;
		break;
	    case 'm':
		tmpMonk = new File(opt.getOptArg());
		break;
	    case 'x':
		tmpXsc = new File(opt.getOptArg());
		break;
	    }
	}
	// Copy "-l" args to loads[].
	int loadn = loadv.size();
	String[] loads = new String[loadn];
	for (int i = 0; i < loadn; i ++)
	    loads[i] = (String) loadv.get(i);

	// Copy remaining arguments to sscs[].
	int oind = opt.getOptInd();
	String[] sscs = new String[args.length - oind];
	for (int i = 0; i < sscs.length; i ++)
	    sscs[i] = args[i + oind];

	if (sscs.length < 1)
	{
	    // Que?
	    System.err.println("Usage:  "
		+ "Ssc2xsc.main [-j [-u]] [-i indir] [-o outdir] {-l loaddir} "
		+ "[-d dirprefix] [-p packprefix] [-m monk.tmp] [-x xsc.tmp] "
		+ "file.ssc ...");
	    System.exit(1);
	}
	try
	{
	    SscConv.debug = debug;
	    SscConv.convert2(idir, odir, (loads), (sscs), prepack,
		null, tmpMonk, tmpXsc, uconv, sx, ext);
	    for (Iterator i = ext.iterator(); i.hasNext(); )
		sx.append("ALSO " + (String) i.next() + '\n');
	    System.out.print(sx.toString());
	}
	catch (Exception exc)
	{
	    exc.printStackTrace();
	    System.exit(1);
	}
	System.exit(0);
    }

    public static String slashPath (File f)
    {
	return f.getPath().replace('\\', '/');
    }

    // Note: this needs to be changed to support external templates
    public void run
	(String inputSsc, String outputXscFilename, String packageName)
	throws IOException, SAXException
    {
	File issc = new File(inputSsc);
	File oxsc = new File(outputXscFilename);
	File odir = oxsc.getParentFile();
	String[] loads = new String[0];
	String[] sscs = new String[1];
	sscs[0] = issc.getName();
	File otmp = new File(odir, sscs[0]);
	File idir = issc.getParentFile();
	SscConv.convert2(idir, odir, loads, sscs, packageName, "etd",
	    null, null, false, new StringBuffer(), new HashSet());
	if (otmp.compareTo(oxsc) != 0 && ! otmp.renameTo(oxsc))
	    throw new IOException("cannot move \"" + otmp.getPath()
		+ "\" to output XSC file \"" + oxsc.getPath() + "\"");
    }

    // Recursively delete given directory.
    private void delete (File dir)
    {
	File[] contents = dir.listFiles();
	for (int i = 0; i < contents.length; i ++)
	{
	    if (contents[i].isDirectory())
	    {
		delete(contents[i]);
	    }
	    else if (!contents[i].delete())
	    {
		System.err.println("Couldn't delete: "
		    + contents[i].getPath());
	    }
	}
	if (! dir.delete())
	    System.err.println("Couldn't delete: " + dir.getPath());
    }

////////////////////////////////////////////////////////////////////////////////

    static final HashSet javaKeyword = new HashSet();
    static
    {
	String[] key =
	{
	    "abstract", "boolean", "break", "byte", "byvalue", "case",
	    "cast", "catch", "char", "class", "const", "continue",
	    "default", "do", "double", "else", "extends", "false",
	    "final", "finalize", "finally", "float", "for", "future",
	    "generic", "goto", "if", "implements", "import", "inner",
	    "instanceof", "int", "interface", "long", "native", "new",
	    "null", "operator", "outer", "package", "private",
	    "protected", "public", "rest", "return", "short", "static",
	    "strictfp", "super", "switch", "synchronized", "then",
	    "this", "throw", "throws", "transient", "true", "try",
	    "var", "void", "volatile", "while", "widefp"
	};
	for (int i = 0; i < key.length; i ++)
	    javaKeyword.add(key[i]);
    }

    /**
     * Tests if given string is a Java reserved keyword.
     *
     * @param s  the potential keyword
     * @return the test result
     */
    static boolean isJavaKeyword (String s)
    {
	return javaKeyword.contains(s);
    }

    /**
     * Converts relative directory path to similar path, where all directory
     * names have been translated to be useful as Java package names.
     * The POSIX portable character set allows "-_#~." and ASCII letters/digits.
     * Of these characters, Java does not allow "-#~." in identifiers, and does
     * not allow a leading digit.  On some target systems (Windoze) filenames
     * that only differ in case will clash, and the Java convention is to use
     * lower-case names for package names, and we want to rule out consecutive
     * underscores for legibility.  This leaves us with the following name
     * syntax: ([a-z]+(_[a-z0-9]+)*)|(_[a-z0-9]+)+
     * Note that the root itself is denoted by "", not "." (dot).
     * The resulting name will be terminated with "/" (unless empty).
     *
     * @param dir  the original path
     * @return the translated directory path
     */
    private String xscDirPathTranslate (String dir)
    {
	char prev = '/';
	int len = dir.length(), first = 0;
	StringBuffer b = new StringBuffer(len);

	for (int i = 0; i < len; i ++)
	{
	    char c = dir.charAt(i);
	    if (c == '\\')
	    {
		// Normalise for later conversion to package name.
		c = '/';
	    }
	    else if ('A' <= c && c <= 'Z')
	    {
		// Only use lower-case letters.
		c = Character.toLowerCase(c);
	    }
	    else if (c == '_' || c == '-' || c == '.')
	    {
		// Don't use consecutive underscores.
		c = '_';
		if (prev == c)
		    continue;
	    }
	    else if ('0' <= c && c <= '9')
	    {
		// Cannot start with a digit.
		if (prev == '/')
		    b.append('_');
	    }
	    else if (! ('a' <= c && c <= 'z'))
	    {
		// Skip anything else.
		continue;
	    }
	    if (c == '/' && isJavaKeyword(b.substring(first)))
		b.append('_');
	    b.append(c);
	    prev = c;
	}
	if (prev != '/')
	    b.append('/');
	return b.toString();
    }

    private Hashtable s2xMap = new Hashtable(); // cache of SSC->XSC directories
    private Hashtable x2sMap = new Hashtable(); // cache of XSC->SSC directories

    /**
     * Like xscDirPathTranslate(), but caches the results, and makes sure
     * that two different input paths do not map to the same normalised
     * output directory path.
     *
     * @param sscDir  the original path
     * @return the converted, unique, Posix-compliant path
     */
    private String xscDirUnique (String sscDir)
    {
	String xscDir = (String) s2xMap.get(sscDir);
	if (xscDir == null)
	{
	    xscDir = xscDirPathTranslate(sscDir);
	    String copy = (String) x2sMap.get(xscDir);
	    if (copy != null)
		throw new RuntimeException("SSC dirs \" + copy + \" and \""
		    + sscDir + "\" yield the same XSC dir \"" + xscDir + "\"");
	    // New entry.
	    s2xMap.put(sscDir, xscDir);
	    x2sMap.put(xscDir, sscDir);
	}
	return xscDir;
    }

    /**
     * Converts directory path to package name.
     * This assumes the filename separator has been normalised to '/'.
     *
     * @param prefix  prepend to package
     * @param dir  normalized directory name
     * @return pakcage corresponding to directory, below prefix
     */
    private static String xscPackage (String prefix, String dir)
    {
	if (dir == null) { dir = ""; }
	String result = prefix + dir.replace('/', '.');
	return result.endsWith(".")
	    ? result.substring(0, result.length() - 1)
	    : result;
    }
}
