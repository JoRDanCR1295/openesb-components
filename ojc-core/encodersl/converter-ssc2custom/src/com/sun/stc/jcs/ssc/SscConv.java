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
 * @(#)SscConv.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.io.*;
import java.util.*;  // ESR 104056   
import org.xml.sax.SAXException;


import com.sun.stc.jcs.JCSCompiler;
import com.sun.stc.jcs.JCSConstants;
import com.sun.stc.jcs.ssc.Monk.*;
import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.StringCoderFactory;

/**
 * Code to convert SSC to XSC files.
 * Part of the e*Gate 4.5 middleware product.
 */
public class SscConv
{
    public static boolean debug = false;

    /**
     * Flag to force all Java names to ASCII.
     * This is useful if the platform or locale has problems supporting
     * the set of Unicode characters used in the generated code.
     * Initialized from the boolean JCS property "SscConv.ascii".
     */
    public boolean asciiOnly =
	JCSProperties.getFlag("SscConv.ascii", false);

    /**
     * Flag to revert to XSC 0.3 style.
     * Used to generate XSC files that can be used with e*Gate 4.5.
     * Initialized from the boolean JCS property "SscConv.xsc03".
     */
    public boolean xsc03 =
	JCSProperties.getFlag("SscConv.xsc03", false);
    /**
     * Flag to force output of the "defaultBytes" attribute for each
     * &lt;node&gt; entitiy in the XSC file that also has a "defaultValue"
     * attribute value.
     * Used to check that the preservation of the encoding is working correctly.
     * Initialized from the boolean JCS property "SscConv.bytes".
     */
    public boolean defBytes =
	JCSProperties.getFlag("SscConv.bytes", false);

    /**
     * Flag to add the required &lt;throws&gt; entities to the &lt;methods&gt;
     * entities in the XSC file.  Used for full XSC 0.6 compliance.  Note that
     * the e*Gate 4.5.1 ETD editor will choke on these.
     * Initialized from the boolean JCS property "SscConv.addthrows".
     */
    public boolean addThrows =
	JCSProperties.getFlag("SscConv.addThrows", false);

    /**
     * Flag to allow safe spaces in normal-safe escaped attribute strings.
     * When set, will leave single embedded spaces as spaces in the XSC file,
     * because it will not be affected by XML attribute normalization.
     * Initialized from the boolean JCS property "SscConv.space".
     */
    static boolean showSpace =
	JCSProperties.getFlag("SscConv.space", true);

    /**
     * Maximum class nesting depth, beyond this generate shallow nesting.
     * When set to a non-zero value, composite nodes with a static nesting
     * level (below a template root) greater than this value will be generated
     * with an explicit "javaType" for a generated class inside a fixed
     * container class.
     */
    static int snest =
	JCSProperties.getInteger("SscConv.snest", 0);

    /**
     * Temporary flag to allow Nt/Ex/Gr modifiers.
     * Should not activate until QA 34149 approved.
     * Initialized from the boolean JCS property "SscConv.qa34149".
     */
    static boolean qa34149 =
	JCSProperties.getFlag("SscConv.qa34149", false);

    /**
     * Character used as marker in the XSC generation.
     * This should be a character we would never generate into an XSC file.
     */
    private static final char METHMARK = '\b';

    /**
     * Constructs with no arguments.
     */
    public SscConv () { }

    /**
     * Reports given error as IllegalArgumentException, with location info.
     * The error must be related to some item of converted Monk input.
     *
     * @param m  any Monk data item, to carry origin
     * @param msg  the error text string
     */
    private static void die (MonkData m, String msg)
	throws IllegalArgumentException
    {
	String file = m.loc.getFile();
	int    line = m.loc.getLine();
	throw new IllegalArgumentException
	    ((file == null ? "" : ("file " + file + ": "))
	    + (line == 0 ? "" : ("line " + line + ": "))
	    + msg);
    }

    /**
     * Compares two MonkData values for content equivalence.
     * This has only been implemented for a few primitive types;
     * all non-primitive types are compared for object identity.
     * Null values are accepted, they are only equivalent to null.
     *
     * @param m1  1st Monk data item (or null)
     * @param m2  2nd Monk data item (or null)
     * @return the test result
     */
    public static boolean monkEquals (MonkData m1, MonkData m2)
    {
	if (m1 == null)
	    return (m2 == null);
	if (m2 == null)
	    return false;
	if (m1.type != m2.type)
	    return false;
	switch (m1.type)
	{
	case MonkData.BOOLEAN:
	    MonkBoolean mb1 = (MonkBoolean) m1;
	    MonkBoolean mb2 = (MonkBoolean) m2;
	    return (mb1.value == mb2.value);

	case MonkData.INTEGER:
	    MonkInteger mi1 = (MonkInteger) m1;
	    MonkInteger mi2 = (MonkInteger) m2;
	    return
		(mi1.exact == mi2.exact) &&
		(mi1.value == mi2.value);

	case MonkData.STRING:
	    MonkString ms1 = (MonkString) m1;
	    MonkString ms2 = (MonkString) m2;
	    if (ms1.length != ms2.length)
		return false;
	    for (int i = 0; i < ms1.length; i ++)
		if (ms1.value[i] != ms2.value[i])
		    return false;
	    return true;

	case MonkData.SYMBOL:
	    return m1.getSymbol().equals(m2.getSymbol());
	}
	return false;
    }

    /**
     * Class to match Monk data with a pattern.
     * This is used for analysis of Monk data, after reading it in from a
     * file as generic Monk source.
     */
    public static class MonkMatch
    {
	private String desc;
	private MonkData[] out = null;
	private MonkData[] carg = null;
	private String what;
	private int lastIndex = -1;

	/**
	 * Constructs a new match pattern.
	 *
	 * @param what  verbal description, for error message
	 * @param desc  match pattern
	 * @param oargs  output argument count
	 * @param carg  constant input array, to match with
	 */
	public MonkMatch (String what, String desc, int oargs, MonkData[] carg)
	{
	    this.what = what;
	    this.desc = desc;
	    this.carg = carg;
	    this.out = (oargs > 0 ? new MonkData[oargs] : null);
	}

	/**
	 * Reports the number of output arguments found by the match.
	 *
	 * @return a non-negative integer value
	 */
	public int outMax ()
	{
	    return (out == null ? 0 : out.length);
	}

	/**
	 * Gets the Nth output argument found by the match.
	 * Arguments are numbered from 0.
	 *
	 * @param the index value
	 * @return the item of Monk data
	 */
	public MonkData out (int i)
	{
	    if (i < 0 || outMax() <= i)
		throw new IndexOutOfBoundsException("no out(" + i
		    + "), range is 0 through " + (outMax() - 1));
	    return out[i];
	}

	/**
	 * Returns the index of the last matched item.
	 * -1 indicates an empty list.
	 *
	 * @return the index (starting at 0)
	 */
	public int getLastIndex()
	{
	    return this.lastIndex;
	}

	/**
	 * Matches a Monk list to the descriptor.
	 * Returns the matched items via the "out" array.
	 * If not matched, returns non-null, or throws exception if
	 * the "must" flag is set.  The elements are matched as follows:
	 *
	 *  B = boolean,
	 *  I = integer,
	 *  L = pair,
	 *  S = string,
	 *  Y = symbol,
	 *  X = anyting,
	 *  * = rest of list.
	 *
	 * When an element is preceded by '?' it is optional; if preceded
	 * by '-' then skip it.
	 *
	 * @param must  flag: must match or fail?
	 * @param data  input list to match
	 * @param rarg  array of input arguments
	 * @return error message, or null if okay
	 * @throws IllegalArgumentException on mismatch, if "must" set
	 */
	private String match (boolean must, MonkData data, MonkData[] rarg)
	    throws IllegalArgumentException
	{
	    int opos = 0;
	    this.lastIndex = -1;
	    boolean opt = false, skip = false;
	    byte type = MonkData.NONE;
	    int amax = (desc == null ? -1 : (desc.length() - 1));
	    int elem = 0;
	    MonkData a = null;
	    for (int i = 0; i < desc.length(); i ++)
	    {
		char c = desc.charAt(i);
		switch (c)
		{
		case '?':
		    // Next is optional.
		    opt = true;
		    continue;
		case '-':
		    // Next is skipped.
		    skip = true;
		    continue;

		case 'B':
		    type = MonkData.BOOLEAN;
		    break;
		case 'I':
		    type = MonkData.INTEGER;
		    break;
		case 'L':
		    type = MonkData.PAIR;
		    break;
		case 'S':
		    type = MonkData.STRING;
		    break;
		case 'Y':
		    type = MonkData.SYMBOL;
		    break;
		case 'X':
		    type = MonkData.NONE;
		    break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		    // Match particular item.
		    int anum = c - '0';
		    if (anum > amax)
			throw new RuntimeException("no input arg #" + anum);
		    a = rarg[anum];
		    type = a.type;
		    break;

		case '*':
		    // Rest of list.
		    if (! skip) {
			out[opos] = data;
			this.lastIndex = opos++;
		    }
		    continue;

		case ')':
		    // End of list.
		    if (! data.isNil())
		    {
			if (must)
			    die(data, what + ": trailing junk in list");
			return "trailing junk in list";
		    }
		    continue;

		default:
		    // Huh?
		    throw new RuntimeException("MonkMatch: <" + c + "> ?");
		}
		// Expecting data of particular type.
		if (data.isPair())
		{
		    MonkData m = data.getCar();
		    if ((type == MonkData.NONE || m.type == type) &&
			(a == null || monkEquals(m, a)))
		    {
			// We have a match.
			if (! skip)
			{
			    // Yield as output.
			    if (opos >= outMax())
				throw new RuntimeException("MonkMatch: "
				    + "over " + outMax()
				    + " outputs in " + what);
			    out[opos] = m;
			    this.lastIndex = opos++;
			}
			/*-
			System.out.println("[ consume " + elem
			    + " as <" + c + "> ]");
			-*/
			data = data.getCdr();
			elem ++;
		    }
		    else if (opt)
		    {
			// Optional element missing.
			if (! skip)
			{
			    // Yield as output.
			    if (opos >= outMax())
				throw new RuntimeException("MonkMatch: "
				    + "over " + outMax()
				    + " outputs in " + what);
			    out[opos] = null;
			    this.lastIndex = opos++;
			}
			/*-
			System.out.println("[ missing " + elem
			    + " as <" + c + "> ]");
			-*/
		    }
		    else
		    {
			// Data mismatched.
			if (! must) { return "type mismatch"; }
			die(data, "match " + what + ": expected "
			    + MonkData.typeName(type)
			    + ", but element " + elem + " is " + m.typeName());
		    }
		}
		else if (! opt)
		{
		    // Data structure is not a proper list (pair chain).
		    if (must)
		    {
			if (data.isSymbol()) {
			    die(data, "extra element named "
				+ ((MonkSymbol)data).value
				+ " encountered."
			    );

			} else {
			    die(data, "match " + what + ": invalid element"
				+ " of type " + data.typeName()
				+ " as list tail, expected a list element (type "
				+ c + ")"
			    );
			}
		    }
		    return "not a list";
		}
		opt = skip = false;
	    }
	    return null;
	}
    }

    /**
     * Given a name, checks whether its canonicized form is in one
     * of the given two hashsets.  The canonization is required
     * to protect against filename conflicts on Windoze.
     *
     * @param name  the name to check
     * @param h1  1st name collection to check
     * @param h2  2nd name collection to check
     * @return the test result
     */
    public static boolean seenJavaId (String name, HashSet h1, HashSet h2)
    {
	String canon = name.toLowerCase();
	return
	    (h1 != null && h1.contains(canon)) ||
	    (h2 != null && h2.contains(canon));
    }

    /**
     * Adds a canonicized version of this name to the given hashset.
     *
     * @param name  a Java identifier
     * @param used  the name collection to register the name in
     */
    public static void addJavaId (String name, HashSet used)
    {
	String canon = name.toLowerCase();
	used.add(canon);
    }

    /**
     * Removes the canonicized version of this name from the given hashset.
     *
     * @param name  a Java identifier
     * @param used  the name collection to unregister the name from
     */
    public static void removeJavaId (String name, HashSet used)
    {
	String canon = name.toLowerCase();
	used.remove(canon);
    }

    /**
     * Tests if the given string contains a valid Java identifier,
     * and is within the constraints for XSC "javaName" values.
     * The name must be a so-called "bean" access name, i.e. not start
     * with a lower-case ASCII letter.
     *
     * @param name  the name string
     * @return the test result
     */
    public static boolean isJavaId (String name)
    {
	if (name == null)
	    return false;
	int len = name.length();
	if (len == 0 || isJavaKeyword(name))
	    return false;
	char first = name.charAt(0);
	if (('a' <= first && first < 'z') ||
	    first == '$' ||
	    ! Character.isJavaIdentifierStart(first))
		return false;
	for (int i = 1; i < len; i ++)
	    if (! Character.isJavaIdentifierPart(name.charAt(i)))
		return false;
	return true;
    }

    /**
     * Given a string, transforms it into a legal Java identifier.
     * Characters that are not legal are removed; it the 1st
     * char would be legal as 2nd but not as 1st, prefix "_".
     * If identifier is in list of reserved words, prefix "_".
     * If no legal chars remain, return "_".
     * In any case, check the resulting name against the "used"
     * list of strings; make sure the resulting name does not
     * occur there, appending a "_xN" (N=0...) suffix as necessary.
     *
     * @param str  the original name to transform
     * @param HashSet used1  1st set of names not to use
     * @param HashSet used2  2nd set of names not to use
     * @param boolean ascii  flag: force Java name to pure ASCII?
     */
    public static String makeJavaId
	(String str, HashSet used1, HashSet used2, boolean ascii)
    {
	int len = str.length();
	StringBuffer out = new StringBuffer(len);
	int low = 1;
	int pos = 1;
	boolean non = true;  // previous char suppressed, or 1st char?
	boolean big = false; // previous char A-Z or 0-9?

	for (int i = 0; i < len; i ++)
	{
	    char c = str.charAt(i);
	    if (c == '_' || c == '$' || ! Character.isJavaIdentifierPart(c))
	    {
		// Skip it.
		non = true;
		continue;
	    }
	    if (ascii && c > 0x7F)
	    {
		// Force to ASCII-only name, by escaping to "_Uxxxx".
		out.append("_U");
		numOut(out, c, 4, 16);
		non = big = true;
		continue;
	    }
	    // Valid character, add to string.
	    if (non && big)
	    {
		// Last fragment ended A-Z/0-9, insert underscore.
		out.append('_');
	    }
	    /* Special rule (Java-Bean): if 1st char is a-z, change to A-Z.
	     * Guess this means we could skip tests for most Java keywords
	     * and prefixes...  [MJDL 2001-04-12]
	     */
	    if (non && 'a' <= c && c <= 'z')
		c = (char)(c - 'a' + 'A');
	    out.append(c);
	    non = false;
	    big = (('A' <= c && c <= 'Z') || ('0' <= c && c <= '9'));
	}
	if (out.length() == 0 ||
	    (out.length() > 2 &&
		(out.substring(0, 3).equals("get") ||
		out.substring(0, 3).equals("set"))) ||
	    ! Character.isJavaIdentifierStart(out.charAt(0)) ||
	    isJavaKeyword(out.toString()))
	{
	    // Some kind of clash; prefix underscore.
	    out.insert(0, '_');
	}
	String result = out.toString();
	String origin = result;
	for (int i = 0 ; seenJavaId(result, used1, used2); i ++)
	    result = origin + "_x" + (i == 0 ? "" : Integer.toString(i));
	return result;
    }

    /**
     * Prefix for dumb conversion of external reference paths in SSC
     * files to the same path in XSC files.  When not null, this serves
     * as a flag to switch off really looking for external references.
     */
    public String dumbSscRoot = null;

    // Root of SSC tree; used for relative search path in references.
    private File sscInputRoot = null;

    /* Ordered list of directories relative to sscInputRoot, comprising the
     * search path to resolve global template references in input SSC files.
     */
    private String[] sscSearch = null;

    // List of load-path mappings (original, normalized).
    private Hashtable loads = null;

    /**
     * Given a filename, as used in an input SSC file, finds it along
     * the Monk load path specified.  Registers the mapping found in the
     * global "loads" mapping.  Returns the path relative to the input
     * SSC root, or null if not found.
     * <p>
     * SSC files contain a "load" statement for each external
     * template.  Note that they follow any set-file-encoding-method
     * calls in the SSC files generated by the Monk ETD editor,
     * but we ignore the encoding for now...
     * <p>
     * If the global "dumbSscRoot" is set, then just prefix this to
     * translate the reference.
     *
     * @param ssc  the unnormalized SSC file path
     */
    private String normaliseLoad (String ssc)
    {
	String xsc;
	if (loads == null)
	    loads = new Hashtable();

	if (dumbSscRoot != null)
	{
	    // Dumb conversion.
	    xsc = dumbSscRoot + '/' + ssc;
	}
	else
	{
	    // Really look it up.
	    if (sscSearch.length == 0)
		throw new RuntimeException(
		    "no load-path to resolve reference [" + ssc + "]");
	    xsc = (String) loads.get(ssc);
	    for (int i = 0; xsc == null && i < sscSearch.length; i ++)
	    {
		File dir = new File(sscInputRoot, sscSearch[i]);
		if ((new File(dir, ssc)).exists())
		{
		    String part = sscSearch[i];
		    if (part.equals("")) { part = null; }
		    xsc = Ssc2xsc.slashPath(new File(part, ssc));
		    while (xsc.startsWith("./"))
			xsc = xsc.substring(2);
		    break;
		}
	    }
	    if (xsc == null)
	    {
		// Not found.
		return null;
	    }
	}
	loads.put(ssc, xsc);
	if (debug)
	    System.out.println("[ load <" + ssc + "> = <" + xsc + "> ]");
	return xsc;
    }

    /**
     * Converts load-path name to normalised load path, or null if not found.
     * We expect the global list "loads" to map internal path references to
     * normalised references (both strings).
     * Strips the usual ".ssc" suffix.
     *
     * @param data  for input line reference
     * @param ssc  the unnormalized SSC file path
     */
    private String normalisePath (MonkData data, String path)
    {
	if (debug)
	    System.out.println("[ normalise <" + path + "> ]");
	String result = null;
	if (loads != null)
	    result = (String) loads.get((Object) path);
	if (result == null)
	    die(data, "using file reference \"" + path
		+ "\" without prior load clause");
	if (result.endsWith(".ssc"))
	    result = result.substring(0, result.length() - 4);
	return result;
    }

    private int uidNumber = 0; // next uid() value

    /**
     * Generates a file-wide unique UID.
     * <p>
     * NYI: should replace by SSC_File.getUid() calls...
     *
     * @return a UID string
     */
    public String uid ()
    {
	return Integer.toString(uidNumber ++);
    }

    /**
     * Converts internal field name symbol to something permissible as
     * a logical name.  Requires the list of names already used below
     * the given parent, to disambiguate equivalent names.
     *
     * @param name  original field name
     * @param used1  1st list of names already in use
     * @param used2  2nd list of names already in use
     * @return the disambiguated, transformed name
     */
    private String sscFieldName (MonkSymbol name, HashSet used1, HashSet used2)
    {
	return makeJavaId(name.value, used1, used2, asciiOnly);
    }

    /**
     * Outputs non-negative N-digit number to string buffer.
     *
     * @param sb  buffer to emit string
     * @param num  the number to emit
     * @param len  number of digits
     * @param base  numeric base, in range 2-16
     */
    private static void numOut (StringBuffer sb, int num, int len, int base)
    {
	char[] c = new char[len];
	for (; len-- > 0; num /= base)
	    c[len] = "0123456789abcdef".charAt(num % base);
	sb.append(c);
    }

    /**
     * Converts output data character to escaped XML string.
     * This uses the so-called "normal-safe" XSC format, in which
     * anything other that printable ASCII is rendered as backslash +
     * "u" + 4-digit hex code.  This form is impervious to attribute
     * normalisation, but does require encoding/decoding.
     *
     * @param sb  buffer to emit string
     * @param c  Unicode character value
     */
    public static void sscNormal (StringBuffer sb, int c)
    {
	boolean esc = false;
	switch (c)
	{
	case '"':
	case '<':
	case '>':
	case '&':
	case '\'':
	case '\\':
	    esc = true;
	    break;
	default:
	    esc = (c < 0x21 || 0x7E < c);
	}
	if (esc)
	{
	    sb.append('\\');
	    sb.append('u');
	    numOut(sb, c, 4, 16);
	}
	else
	{
	    sb.append((char) c);
	}
    }

    /**
     * Converts output data character to escaped XML string.
     * Special characters are protected by using character-entity
     * references or XML specials.
     *
     * @param sb  buffer to emit string
     * @param c  Unicode character value
     */
    public static void sscEscape (StringBuffer sb, int c)
    {
	boolean esc = false;
	switch (c)
	{
	case '<':  sb.append("&lt;");   break;
	case '>':  sb.append("&gt;");   break;
	case '&':  sb.append("&amp;");  break;
	case '"':  sb.append("&quot;"); break;
	case '\'': sb.append("&quot;"); break;
	default:
	    if (c < 0x21 || 0x7E < c)
	    {
		sb.append("&#x");
		numOut(sb, c, 4, 16);
		sb.append(";");
	    }
	    else
		sb.append((char) c);
	}
    }

    // Convert output Monk string to normal-safe XML string.
    public static void sscNormal (StringBuffer sb, MonkString s)
    {
	for (int i = 0; i < s.length; i ++)
	    sscNormal(sb, s.value[i]);
    }

    // Convert output Monk string to normal-safe XML string.
    public static String sscNormal (MonkString s)
    {
	StringBuffer sb = new StringBuffer();
	sscNormal(sb, s);
	return sb.toString();
    }

    // Convert output Monk string to escaped XML string.
    public static void sscEscape (StringBuffer sb, MonkString s)
    {
	for (int i = 0; i < s.length; i ++)
	    sscEscape(sb, s.value[i]);
    }

    // Convert output data string to normal-safe XML string.
    public static void sscNormal (StringBuffer sb, String s)
    {
	int last = s.length() - 1;
	if (showSpace)
	{
	    // Special improvement mode: show single spaces as such in output.
	    boolean spc = true;
	    for (int i = 0; i < last; i ++)
	    {
		char c = s.charAt(i);
		if (! spc && c == ' ')
		    { sb.append(' '); spc = true; }
		else
		    {	if (c == '\\' && s.charAt(i+1) == 'u')
		    	{	sb.append(c); spc = false; }
				else
				{ sscNormal(sb, c); spc = false; }
			}
	    }
	    if (last >= 0)
		sscNormal(sb, s.charAt(last));
	}
	else
	{
	    // Regular brand.
	    for (int i = 0; i <= last; i ++)
		sscNormal(sb, s.charAt(i));
	}
    }

    // Convert output data string to normal-safe XML string.
    public static String sscNormal (String s)
    {
	StringBuffer sb = new StringBuffer();
	sscNormal(sb, s);
	return sb.toString();
    }

    // Convert output data string to escaped XML string.
    public static void sscEscape (StringBuffer sb, String s)
    {
	int len = s.length();
	for (int i = 0; i < len; i ++)
	    sscEscape(sb, (int) s.charAt(i));
    }

    // Convert output data string to escaped XML string.
    public static String sscEscape (String s)
    {
	StringBuffer sb = new StringBuffer();
	sscEscape(sb, s);
	return sb.toString();
    }

    private MonkMatch mdes = null;

    /**
     * Class used for generating standard method info.
     * The so-called standard methods are a collection of methods
     * automatically added to the top-level node of an ETD by the back-end.
     */
    static class XscMethod
    {
	private Emit e = null;
	private SSC_File file;
	private boolean doThrow;
	private Vector info;
	private StringBuffer sig = new StringBuffer();

	/**
	 * Creates with output buffer and option SSC_File for the UIDs.
	 * If file is null, don't generate UIS.
	 * The doThrow flag determines if "throws" information will be output.
	 *
	 * @param e  output stream
	 * @param file  dummy for UID generation, or null
	 * @param doThrow  flag: emit "throws" attributes?
	 */
	public XscMethod (Emit e, SSC_File file, boolean doThrow)
	{
	    this.e = e;
	    this.file = file;
	    this.doThrow = doThrow;
	    this.info = new Vector();
	}

	/**
	 * Adds exception information, as a &lt;throws&gt; entity.
	 * Must be called before the method() that uses it.
	 *
	 * @param _className  the exception class name
	 * @param comm  comment to add, or null
	 */
	public void exception (String _className, String comm)
	{
	    if (comm.equals("")) { comm = null; }
	    if (doThrow)
	    {
		info.add("<throws excepType=\"" + sscEscape(_className) + "\""
		    + " comment=\"" + sscNormal(comm) + "\""
		    + (file!=null?(" uid=\"" + file.makeUid()+ "\""):"")
		    + "/>");
	    }
	}

	/**
	 * Adds parameter information, as a &lt;param&gt; entity.
	 * Must be called before the method() that uses it.
	 *
	 * @param name  the parameter name
	 * @param type  the fully qualified Java type of the parameter
	 * @param comm  comment to add, or null
	 */
	public void param (String name, String type, String comm)
	{
	    if (comm.equals("")) { comm = null; }

	    info.add("<param name=\"" + sscEscape(name) + "\""
		+ " paramType=\"" + sscEscape(type) + "\""
		+ (comm!=null?(" comment=\"" + sscNormal(comm) + "\""):"")
		+ (file!=null?(" uid=\"" + file.makeUid() + "\""):"")
		+ "/>"
	    );

	    sig.append(JCSCompiler.typeSignature(type));
	}

	/**
	 * Adds a &lt;method&gt; entity, including all the information passed in
	 * prior calls to param() and exception().  The param/exception
	 * information is cleared by this call.
	 *
	 * @param name  the method name
	 * @param type  the fully qualified Java return type
	 * @param comm  comment to add, or null
	 */
	public void method (String name, String type, String comm)
	{
	    int size = info.size();
	    if (comm.equals("")) { comm = null; }

	    e.emit("<method name=\"" + sscEscape(name) + "\""
		+ " returnType=\"" + sscEscape(type) + "\""
		+ " signature=\"" + sscEscape(name) + "(" + sig.toString() + ")"
		+ JCSCompiler.typeSignature(type) + "\""
		+ (comm!=null?(" comment=\"" + sscNormal(comm) + "\"") : "")
		+ (file!=null?(" uid=\"" + file.makeUid() + "\"") : "")
		+ (size==0?"/>":">")
	    );
	    if (size > 0)
	    {
		e.indent();
		for (int i = 0; i < size; i ++)
		    e.emit((String) info.get(i));
		e.done("</method>");
		info.clear();
	    }
	    sig.setLength(0);
	}

	// Add method, no comment.
	public void method (String name, String type)
	    { method(name, type, null); }
    }

    /**
     * Tests if this byte-array contains any values outside range 0-126.
     *
     * @param b  a (possibly null) byte-array
     * @return the test result
     */
    private static boolean containsNonAscii (byte[] b)
    {
	if (b != null)
	    for (int i = 0; i < b.length; i ++)
		if (b[i] < 0 || 126 < b[i])
		    return true;
	return false;
    }

    private static int crunchSeqNo = 0;
    private String crunchPackage = null;

    /**
     * String to use as prefix for "crunched", de-nested node classes.
     * Used when SscConv.snest is non-zero, as prefix for the basename
     * of the explicit class names used for the "javaType" attribute
     * generated for deeply nested composite nodes.
     * Initialized from the JCS property "SscConv.crunch", defaults to "Crunch".
     */
    private String crunchPrefix =
	JCSProperties.getProperty("SscConv.crunch", "Crunch");

    /**
     * Generates ETD-wide unique name, used for de-nested composite field.
     *
     * @return the class name
     */
    private String getCrunchName (String javaName)
    {
	return (crunchPackage == null ? "" : (crunchPackage + '.'))
	    + crunchPrefix + (crunchSeqNo ++) + '_' + javaName;
    }

    /**
     * Generates a single delimiter descriptor from a Monk string.
     *
     * @param str  Monk string literal token
     * @return the XSC delimiter descriptor
     */
    public Xsc.OneDelim makeOneDelim (MonkData str)
    {
	if (str == null || ! str.isString())
	    throw new IllegalArgumentException
		("makeOneDelim: non-string input");
	MonkString ms = (MonkString) str;
	Xsc.OneDelim result = null;
	try
	{
	    result = new Xsc.OneDelim(null, uid(), null, //- ms.bytes,
		(ms.value == null ? null : new String(ms.value)),
		GenSsc.monk2javaCode(ms.code));
	}
	catch (UnsupportedEncodingException ue)
	{
	    die(str, "delimiter encoding \"" + ms.code
		+ "\" not mappable to Java");
	}
	return result;
    }

    /**
     * Generates a single escape delimiter descriptor from a Monk string.
     *
     * @param str  Monk string literal token
     * @return the XSC escape delimiter descriptor
     */
    public Xsc.Escape makeEscape (MonkData str)
    {
	if (str == null || ! str.isString())
	    throw new IllegalArgumentException
		("makeEscape: non-string input");
	MonkString ms = (MonkString) str;
	Xsc.Escape result = null;
	try
	{
	    result = new Xsc.Escape(null, uid(), null, //- ms.bytes,
		(ms.value == null ? null : new String(ms.value)),
		GenSsc.monk2javaCode(ms.code));
	}
	catch (UnsupportedEncodingException ue)
	{
	    die(str, "escape delimiter encoding \"" + ms.code
		+ "\" not mappable to Java");
	}
	return result;
    }

    /**
     * Given a list that describes a field and its subfields in an SSC file,
     * emits an XSC description for the field, and recursively for all of its
     * subfields. The normal syntax for a (non-template) field list is:
     *
     *   (name type lo hi in out offset length [subfield...])
     *
     * with the following meaning:
     *
     *   type (ON,OF,OS,AS,...) -- kind of node: delimited, fixed, set...
     *   lo (integer)  -- lower bound on repetition
     *   hi (integer or INF)  -- upper bound on repetition (INF=no limit)
     *   in (string or und) -- input pattern (und=no match)
     *   out (string or und) -- output default data (und = none)
     *   offset (integer or und) -- offset (und=after previous)
     *   length (integer) -- total length (0=sum of subs, -1=to end)
     *
     * Preceding the name field may be a sublist of properties.
     *
     * Template fields have a slightly different syntax :
     *   (name type lo hi file struct-name und und)
     * where "file" is the name of a file containing the template.
     *
     * Note: we make the hard assumption that the input has been checked to be
     * a proper list (cf. ssc-proper-list), to minimise input format checking.
     * The "used" and "opas" parameters must be a non-empty lists of names *not*
     * to use; this function will add the generated name to the list.
     * If "top?" is set, the field must not be a leaf node; add fake parent if
     * required to enforce this.
     *
     * @param e  the output stream
     * @param fdes  field to translate
     * @param dent  current indentation
     * @param used  Java names of siblings
     * @param opas  Java names of ancestors
     * @param depth  nesting below template root (0=top-level)
     * @return whether a &lt;node&gt; entity was emitted
     * @throws IllegalArgumentException on invalid or unsupported SSC input
     */
    public boolean convField
	(Emit e, MonkData fdes, HashSet used, HashSet opas, int depth)
	throws IllegalArgumentException, UnsupportedEncodingException
    {
	boolean fail = false;
	boolean top = (depth == 0);

	// Get list elements.
	if (mdes == null)
	    mdes = new MonkMatch("field description", "?LYYXXXXXX*", 10, null);
	try
	{
	    mdes.match(true, fdes, null);
	}
	catch (IllegalArgumentException ex)
	{
	    // Create a more specific message.
	    ex.printStackTrace();

	    // Generate corresponding error message.
	    StringBuffer buf = new StringBuffer("List too short.  Missing");
	    switch (mdes.getLastIndex())
	    {
		case -1: buf = new StringBuffer("Empty list encountered."); break;
		case 1: buf.append(" all after node Name"); break;
		case 2: buf.append(" all after node Type"); break;
		case 3: buf.append(" all after node MinRep"); break;
		case 4: buf.append(" all after node MaxRep"); break;
		case 5: buf.append(" all after node InputMatch"); break;
		case 6: buf.append(" all after node DefaultData"); break;
		case 7: buf.append(" all after node ByteOffset"); break;
		case 8: buf.append(" all after node Length"); break;
		case 9: buf.append(" rest of node"); break;

		// Unknown cause, re-throw exception.
		default: throw ex;
	    }

	    // Throw exception with more specific message.
	    buf.append(" ("+ex.getMessage()+")");
	    throw new IllegalArgumentException(buf.toString());
	}

	MonkData   attr = mdes.out(0);
	MonkSymbol name = (MonkSymbol) mdes.out(1);
	MonkSymbol type = (MonkSymbol) mdes.out(2);
	MonkData   rmin = mdes.out(3);
	MonkData   rmax = mdes.out(4);
	MonkData   itag = mdes.out(5);
	MonkData   otag = mdes.out(6);
	MonkData   offs = mdes.out(7);
	MonkData   tlen = mdes.out(8);
	MonkData   rest = mdes.out(9);

	// Optional properties.
	boolean anch = false;
	boolean banc = false;
	boolean eanc = false;
	boolean arry = false;
	boolean erec = false;
	boolean reqd = false;
	boolean sepa = false;
	boolean exac = false;
	boolean grpd = false;
	boolean ntag = false;
	boolean prec = false;
	boolean sout = false;
	MonkString bdel = null;
	MonkString edel = null;
	MonkString scav = null;
	MonkData cmin = null;
	MonkData cmax = null;

	HashSet kids = new HashSet();

	// Strip "-struct" stuff.
	if (top && name.value.endsWith("-struct"))
	    name = new MonkSymbol(name.loc,
		name.value.substring(0, name.value.length() - 7));
	// Infinity = -1.
	if (rmax.isSymbol("INF"))
	    rmax = new MonkInteger(rmax.loc, true, -1);
	if (! rmax.isInteger())
	    die(rmax, "non-integer maxOccurs value");

	// Suppress info-only nodes...
	if (rmax.isInteger(0))
	    return false;

	// Check bounds to make sure they fit in an int.
	if (((MonkInteger)rmin).value > Integer.MAX_VALUE) {
	    die(rmin, "min-rep value too large");
	}
	if (((MonkInteger)rmax).value > Integer.MAX_VALUE) {
	    die(rmax, "max-rep value too large");
	}
	if (offs.isInteger() && ((MonkInteger)offs).value > Integer.MAX_VALUE) {
	    die(offs, "offset value too large");
	}

	// Fake child to hold data, if top is leaf.
	if (top && (rest == null || rest.isNil()))
	{
	    // (name type 1 1 und und und 0)
	    MonkLoc loc = fdes.loc;
	    MonkNil nil = new MonkNil(loc);
	    MonkInteger one = new MonkInteger(loc, true, 1);
	    MonkInteger zero = new MonkInteger(loc, true, 0);
	    MonkSymbol und = new MonkSymbol(loc, "und");
	    rest = new MonkPair(loc, name,
		new MonkPair(loc, type,
		    new MonkPair(loc, one,
			new MonkPair(loc, one,
			    new MonkPair(loc, und,
				new MonkPair(loc, und,
				    new MonkPair(loc, und,
					new MonkPair(loc, zero,
					    nil))))))));
	    rest = new MonkPair(loc, rest, nil);
	}
	String javaName = sscFieldName(name, used, opas);
	addJavaId(javaName, used);
	addJavaId(javaName, opas);
	String javaText = "";
	if (! javaName.equals(name.value))
	{
	    // The Java-ID is nor the same as the original SSC name.
	    StringBuffer jn = new StringBuffer(" javaName=\"");
	    sscEscape(jn, javaName);
	    jn.append('"');
	    javaText = jn.toString();
	}

	// Process the optional field attributes.
	boolean isParent = rest.isPair();
	byte flags = 0;
	Xsc.Delim delim = new Xsc.Delim(null, uid(), flags);
	//	Xsc.DelimGroup first = new Xsc.DelimGroup(null, null);
	Vector bDelim = new Vector();
	Vector eDelim = new Vector();
	boolean locDelim = false; // got local delimiter?
	int nodeType;
	for (; attr != null && attr.isPair(); attr = attr.getCdr())
	{
	    MonkData a = ((MonkPair) attr).car;
	    if (a.isSymbol())
	    {
		// Simple property.
		String sym = a.getSymbol();
		if (sym.equals("anchored"))
		  {
		    flags |= Xsc.Delim.ANCHORED;
		    locDelim = true;
		  }
		else if (sym.equals("beginanchored"))
		  {
		    flags |= Xsc.Delim.BEGINANCH;
		    locDelim = true;
		  }
		else if (sym.equals("endanchored"))
		  {
		    flags |= Xsc.Delim.ENDANCH;
		    locDelim = true;
		  }
		else if (sym.equals("array"))
		    flags |= Xsc.Delim.ARRAY;
		else if (sym.equals("endofrec"))
		  {
		    flags |= Xsc.Delim.ENDOFREC;
		    locDelim = true;
		  }
		else if (sym.equals("required"))
		  {
		    flags |= Xsc.Delim.REQUIRED;
		    locDelim = true;
		  }
		else if (sym.equals("separator"))
		  flags |= Xsc.Delim.SEPARATOR;
		else if (sym.equals("Bd"))
		    {} // Monk ETD editor bug; ignore lone "Bd"
		else if (sym.equals("Ed"))
		    {} // Monk ETD editor bug; ignore lone "Ed"
		else if (sym.equals("Ex"))
		  exac = true;
		else if (sym.equals("Gr"))
		  grpd = true;
		else if (sym.equals("Nt"))
		  ntag = true;
		else if (sym.equals("Pp"))
		  prec = true;
		else
		    die(a, "unconvertible modifier [" + sym + "]");
	    }
	    else if (a.isPair())
	    {
		// Property with arguments.
		MonkPair a2 = (MonkPair) a;
		if (a2.car.isSymbol())
		{
		    String sym = a2.car.getSymbol();
		    MonkData arg1 = null;
		    if (a2.cdr.isPair())
		    {
			arg1 = a2.cdr.getCar();
		    }
		    if (sym.equals("Bd") || sym.equals("Ed"))
		    {
			/* Modifier "Bd" (begin delimiter) is followed by
			 * one or more delimiters, and optionally by "Ed"
			 * (end delimiter) followed by some delimiters.
			 * If "Ed" is present, the Bd/Ed values form a
			 * separate delimiter group.
			 */
			Xsc.DelimGroup group = new Xsc.DelimGroup(null, null);
			boolean begin = sym.equals("Bd");
			for (MonkData a3 = a2.cdr; a3 != null; a3 = a3.getCdr())
			{
			    if (a3.isNil()) { break; }
			    if (! a3.isPair())
				die(a, "malformed Bd modifier, tail is "
				    + a3.typeName());
			    MonkData a4 = ((MonkPair) a3).car;
			    if (a4.isSymbol("Ed"))
			    {
				// Paired delimiter mark.
				begin = false;
			    }
			    else if (a4.isString())
			    {
				// Explicit delimiter: string value.
				MonkString dval = (MonkString) a4;
				if (dval.length < 1)
				    die(a4, "delimiter cannot be empty string");
				Xsc.OneDelim one = makeOneDelim(dval);
				if (begin)
				    group.addBegin(one);
				else
				    group.addEnd(one);
			    }
			    else if (a4.isPair())
			    {
				// Implicit delimiter: offset[,length] list.
				die(a4, "local encoded delimiters not "
				    + "supported (yet)");
			    }
			    else
			    {
				// Huh?
				die(a4, "unexpected " + a4.typeName()
				    + " in Bd modifier");
			    }
			}
			if (group.getBegins() != null ||
			    group.getEnds() != null)
			{
			    delim.addGroup(group);
			    locDelim = true;
			}
		    }
		    else if (sym.equals("EscD"))
		    {
			/* Modifier "EscD" (escape delimiter) is followed by
			 * one or more escape delimiters.
			 */
			for (MonkData a3 = a2.cdr; a3 != null; a3 = a3.getCdr())
			{
			    if (a3.isNil()) { break; }
			    if (! a3.isPair())
				die(a, "malformed EscD modifier, tail is "
				    + a3.typeName());
			    MonkData a4 = ((MonkPair) a3).car;
			    if (a4.isString())
			    {
				MonkString dval = (MonkString) a4;
				if (dval.length < 1)
				    die(a4, "escape delimiter cannot be empty string");
				delim.addEscape(makeEscape(dval));
			    }
			    else
			    {
				// Huh?
				die(a4, "unexpected " + a4.typeName()
				    + " in EscD modifier");
			    }
			}
		    }
		    else if (sym.equals("Ri"))
		    {
			/* The "Ri" modifier should only occur on array nodes
			 * (ONA/ANA) and defines a local array delimiter.
			 * We don't support that yet, but will accept it empty.
			 */
			if (! a2.cdr.isNil())
			    die(a2, "[Ri] modifier not supported yet");
		    }
		    else if (sym.equals("Sc") || sym.equals("ScN"))
		    {
			if (arg1 == null || ! arg1.isString())
			    die(a2, "[Sc/ScN] scavenger modifier requires "
				+ "string argument");
			scav = (MonkString) arg1;
			sout = sym.equals("Sc");
		    }
		    else if (sym.equals("NofN"))
		    {
			/* The "NofN" has five valid forms:
			 * (NofN lower) -- upper bound is infinity
			 * (NofN lower upper) -- deprecated form
			 * (NofN lower "INF") -- deprecated form
			 * (NofN (lower upper)) -- the normal case
			 * (NofN (lower "INF")) -- allowable explicit infinity
			 */
			if (arg1 == null)
			    die(a2, "[NofN] modifier requires argument");
			if (arg1.isInteger())
			{
			    cmin = arg1;
			    cmax = a2.cdr.getCdr().getCar();
			    if (cmax == null)
				cmax = new MonkSymbol(cmin.loc, "INF");
			}
			else if (arg1.isPair())
			{
			    cmin = arg1.getCar();
			    cmax = arg1.getCdr().getCar();
			    if (cmin == null || cmax == null ||
				! cmin.isInteger())
				    die(a2, "[NofN] invalid argument list");
			}
			else
			{
			    // First argument is not an allowable form.
			    die(a2, "[NofN] requires integer or list, not "
				+ arg1.typeName());
			}
			if (cmin == null || ! cmin.isInteger()) {
			    die(arg1, "[NofN] lower-bound must be integer, "
				+ "not "
				+ (cmin == null ? "empty" : cmin.typeName()));
			}

			if (((MonkInteger)cmin).value > Integer.MAX_VALUE) {
			    die(cmin, "child-min value too large");
			}

			if (cmax.isInteger()
			    && ((MonkInteger)cmax).value > Integer.MAX_VALUE
			) {
			    die(cmax, "child-max value too large");
			}
		    }
		    else
		    {
			// We don't (yet) support e.g. "BdB".
			die(a2, "unconvertible modifier [" + sym + "]");
		    }
		}
		else if (a2.car.isString())
		{
		    /* This must be an abbreviated local delimiter pair,
		     * e.g.:  ((("[" "]") endofrec) name ON ...)
		     */
		    Xsc.DelimGroup group = new Xsc.DelimGroup(null, uid());
		    bdel = null;
		    edel = (MonkString) a2.car;
		    if (a2.cdr.isPair() && a2.cdr.getCar().isString())
		    {
			bdel = edel;
			edel = (MonkString) a2.cdr.getCar();
		    }
		    if (bdel != null)
			group.addBegin(makeOneDelim(bdel));
		    group.addEnd(makeOneDelim(edel));
		    delim.addGroup(group);
		    locDelim = true;
		}
		else
		{
		    // Impossible.
		    die(a, "modifier can't start with " + a2.typeName());
		}
	    }
	    else
	    {
		// We don't (yet) support e.g. "Ri".
		die(a, "modifier must be <symbol>, not <" + a.typeName() + ">");
	    }
	}
	delim.setFlags(flags);

	String tp = type.value;
	if (tp.equals("ON")  ||
	    tp.equals("OF")  ||
	    tp.equals("OS")  ||
	    tp.equals("ONA") ||
	    tp.equals("AN")  ||
	    tp.equals("AF")  ||
	    tp.equals("AS")  ||
	    tp.equals("ANA"))
	{
	    // Normal fields.
	    String order, struc;
	    switch (tp.charAt(0))
	    {
	    case 'O': order = "sequence"; break;
	    case 'A': order = "any"; break;
	    default : order = "?";
	    }
	    switch (tp.charAt(1))
	    {
	    case 'F': struc = "fixed"; break;
	    case 'S': struc = "set"; break;
	    case 'N': struc = (tp.length() == 2 ? "delim" : "array"); break;
	    default : struc = "?";
	    }
	    e.part("<node name=\"" + sscNormal(name.value) + '"' + javaText);
	    if (isParent)
	    {
		// Composite node.
		e.part(" type=\"CLASS\"");
		if (! xsc03 && snest > 0 && depth > snest)
		{
		    // Static nesting too deep.
		    e.part(" javaType=\"" + sscEscape(getCrunchName(javaName))
			+ '"');
		}
	    }
	    else
	    {
		// Simple field node.
		if (! xsc03)
		    e.part(" type=\"FIELD\" javaType=\"java.lang.String\"");
		else
		    e.part(" type=\"java.lang.String\"");
	    }
	    e.part(" order=\"" + order + "\" structure=\"" + struc + "\"");

	    if (rmin.isInteger() && ! rmin.isInteger(1))
	    {
		e.part(" minOccurs=\"" + ((MonkInteger) rmin).value + '"');
	    }
	    if (rmax.isInteger() && ! rmax.isInteger(1))
	    {
		long max = ((MonkInteger) rmax).value;
		e.part(" maxOccurs=\""
		    + (max == -1 ? "unbounded" : Long.toString(max)) + '"');
	    }
	    if (offs.isInteger())
	    {
		e.part(" offset=\"" + ((MonkInteger) offs).value + '"');
	    }
	    else if (! offs.isSymbol("und"))
	    {
		// Huh?
		die(offs, "unsupported field offset format ("
		    + offs.typeName() + ")");
	    }

	    // Figure out the length.
	    String lenstr = "undefined";
	    if (tlen.isInteger())
	    {
		// Simple numeric length.
/*-
		if (debug)
		    System.out.println("[ length is " + tlen.typeName() + " ]");
-*/
		long len = ((MonkInteger) tlen).value;
		if (len != -1) { lenstr = Long.toString(len); }

		if (len>Integer.MAX_VALUE) {
		    die(tlen,"length is too large");
		}
	    }
	    else if (tlen.isPair())
	    {
		// Newer notation for encoded length: list (from, size).
		MonkData from = tlen.getCar();
		MonkData size = tlen.getCdr().getCar();
		if (! from.isInteger() || ! size.isInteger())
		    die(tlen, "expected two integers for encoded length, not "
			+ from.typeName() + " and " + size.typeName());
		long lenoff = ((MonkInteger) from).value;
		if (lenoff>Integer.MAX_VALUE) {
		    die(tlen,"length offset is too large");
		}

		long lensiz = ((MonkInteger) size).value;
		if (lensiz>Integer.MAX_VALUE) {
		    die(tlen,"length size is too large");
		}

		if (lenoff < 0 || lensiz < 1)
		    die(tlen, "invalid values for encoded length ("
			+ lenoff + ", " + lensiz + ")");
		if (lenoff != 0) { e.part(" lengthFrom=\"" + lenoff + '"'); }
		e.part(" lengthSize=\"" + lensiz + '"');
		lenstr = "decimal";
	    }
	    else if (tlen.isString())
	    {
		// Can only be encoded length: "[" number "]".
		String s = tlen.getString();
		int len = s.length();
		if (len < 3 || s.charAt(0) != '[' || s.charAt(len - 1) != ']')
		    die(tlen, "expected brackets for encoded length ["
			+ s + ']');
		s = s.substring(1, len - 1);
		try
		{
		    // Try to parse the number.
		    int lensiz = Integer.parseInt(s);
		    if (lensiz <= 0)
			die(tlen, "illegal encoded length value [" + s + ']');
		    e.part(" lengthSize=\"" + lensiz + '"');
		}
		catch (NumberFormatException ne)
		{
		    die(tlen, "encoded length not a number [" + s + "]: "
			+ ne.getMessage());
		}
		lenstr = "decimal";
	    }
	    else if (! tlen.isSymbol("und"))
	    {
		// Probably encoded length, which we don't support yet...
		die(tlen, "unsupported field length format ("
		    + tlen.typeName() + ")");
	    }
	    e.part(" length=\"" + lenstr + "\"");

	    if (itag.isString() && ((MonkString) itag).length > 0)
	    {
		//NYI: should switch if "ntag" is set
		e.part(" inputMatch=\"" + sscNormal((MonkString) itag) + '"');
	    }
	    if (otag.isString())
	    {
		/* The "output tag" is the 6th part of an SSC field line.
		 * It represents the default output data to be emitted when
		 * a field nor its children have been explicitly set.
		 * In Monk, we require the original byte-sequence from the
		 * string literal reprsentation in the SSC source file.
		 * In XSC, we want to present both the character translation
		 * of those bytes as text (for user information), and the
		 * original bytes with their encoding type, so we can
		 * reproduce the byte sequence in the normalised output-SSC.
		 */
		MonkString oval = (MonkString) otag;
		if (oval.length > 0)
		{
		    e.part(" defaultValue=\"" + sscNormal(oval) + '"');
		    if (! xsc03)
		    {
			/* The "defaultBytes" and "defaultEncoding" attributes
			 * were introduced in XSC 0.4, so suppress for 0.3.
			 */
			byte[] data = null;
			try
			{
			    data = StringCoderFactory.getStringCoder(
				GenSsc.monk2javaCode(oval.code)).encode(
				    otag.getString());
			}
			catch (UnsupportedEncodingException ue)
			{
			    throw new RuntimeException("cannot recode default "
				+ "as [" + oval.code + "]");
			}
			boolean sameAsText = false;
			if (data.length == oval.length)
			{
			    sameAsText = true;
			    for (int i = 0; i < oval.length; i ++)
			    {
				if ((char)(data[i] & 0xFF) != oval.value[i])
				{
				    sameAsText = false;
				    break;
				}
			    }
			}
			if (defBytes || ! sameAsText)
			{
			    // Byte sequence differs from Unicode equivalent.
			    e.part(" defaultBytes=\""
				+ sscNormal(GenXsc.byteString(oval.bytes))
				+ '"');
			}
			if (oval.code != null &&
			    (defBytes || ! oval.code.equals(sscEncoding)))
			{
			    // Needs literal prefix in SSC.
			    e.part(" defaultEncoding=\""
				+ GenSsc.monk2javaCode(oval.code) + '"');
			}
		    }
		}
	    }

	    if (scav != null)
	    {
		e.part(" scavenger=\"" + sscNormal((MonkString) scav) + '"');
		if (sout) { e.part(" scavOutput=\"true\""); }
	    }
	    if (cmin != null)
	    {
		//- assert(cmax !=  null);
		e.part(" childMin=\"" + ((MonkInteger) cmin).value + '"');
		e.part(" childMax=\""
		    + (cmax.isInteger()
			? Integer.toString((int) ((MonkInteger) cmax).value)
			: "unbounded")
		    + '"');
	    }

	    if (exac) { e.part(" exact=\"true\""); }
	    if (grpd) { e.part(" group=\"true\""); }
	    if (ntag) { e.part(" avoidMatch=\"true\""); }
	    if (prec) { e.part(" precedence=\"parent\""); }
	    if (xsc03)
	    {
		if (anch) { e.part(" anchored=\"true\""); }
		if (banc) { e.part(" beginAnchored=\"true\""); }
		if (eanc) { e.part(" endAnchored=\"true\""); }
		if (arry) { e.part(" array=\"true\""); }
		if (erec) { e.part(" endOfRec=\"true\""); }
		if (reqd) { e.part(" required=\"true\""); }
		if (sepa) { e.part(" separator=\"true\""); }
		if (bdel != null)
		{
		    e.part(" beginDelim=\"" + sscNormal(bdel) + '"');
		}
		if (edel != null)
		{
		    e.part(" endDelim=\"" + sscNormal(edel) + '"');
		}
	    }
	    e.part(" uid=\"" + uid() + "\"");

	    // Emit all the subfields.
	    boolean subFields = (isParent || locDelim);
	    if (subFields)
	    {
		e.emit(">");
		e.indent();
		if (locDelim)
		{
		    // Local delimiter info.
		    genXsc.genDelim(delim);
		}
		boolean brood = false;
		for (; rest.isPair(); rest = ((MonkPair) rest).cdr)
		{
		    // Subordinate <node> entities.
		    brood |= convField(e, ((MonkPair) rest).car, kids,
			opas, depth + 1);
		}
		if (isParent && ! brood)
		{
		    // We've already produced a type="CLASS" parent...
		    e.emit("<node name=\"Ersatz\" type=\"FIELD\""
			+ " structure=\"fixed\" order=\"sequence\""
			+ " minOccurs=\"0\" maxOccurs=\"0\""
			+ " comment=\"obligatory child node\"/>");
		}
		e.undent();
	    }
	    else
	    {
		e.emit("/>");
	    }

	    // End of node.
	    if (top)
	    {
		// Insert marker for the "standard" methods.
		e.emi0("" + METHMARK);
	    }

	    if (subFields) { e.emit("</node>"); }
	}
	else if (
	    tp.equals("LTN") ||
	    tp.equals("LTF") ||
	    tp.equals("LTS") ||
	    tp.equals("GTN") ||
	    tp.equals("GTF") ||
	    tp.equals("GTS"))
	{
	    // Template reference.
	    String struc;
	    switch (tp.charAt(2))
	    {
	    case 'F': struc = "fixed"; break;
	    case 'N': struc = "delim"; break;
	    case 'S': struc = "set"; break;
	    default : struc = "?";
	    }
	    String rf = "REFERENCE";
	    String rs = null;
	    String rm = null;
	    if (tp.charAt(0) == 'L')
	    {
		// Local template reference.
		if (! otag.isSymbol())
		    die(otag, "local reference requires symbol");
		rm = otag.getSymbol();
		if (rm.endsWith("-struct"))
		    rm = rm.substring(0, rm.length() - 7);
		if (xsc03)
		{
		    // Use <node type="[name]" ...>
		    rf = rm; rm = null;
		}
		else
		{
		    // Use <node type="REFERENCE" member="[name]" ...>
		}
	    }
	    else
	    {
		// Global template reference.
		if (! itag.isString())
		    die(itag, "global reference requires string");
		rs = normalisePath(itag, new String(((MonkString) itag).value));
	    }
	    e.part("<node name=\"" + sscNormal(name.value)
		+ "\" type=\"" + rf
		+ "\" structure=\"" + struc + '"'
		+ (rs != null ? (" reference=\"" + sscNormal(rs) + ".xsc\"") : "")
		+ (rm != null ? (" member=\"" + sscNormal(rm) + "\"") : ""));
	    if (rmin.isInteger() && ! rmin.isInteger(1))
		e.part(" minOccurs=\"" + ((MonkInteger) rmin).value + '"');
	    if (rmax.isInteger() && ! rmax.isInteger(1))
		e.part(" maxOccurs=\"" + ((MonkInteger) rmax).value + '"');
	    e.emit(" uid=\"" + uid() + "\"/>");
	}
	else
	{
	    // Unknown type?
	    die(type, "unrecognised field type [" + type.value + "]");
	}

	// Remove name from ancestor list.
	removeJavaId(javaName, opas);
	return true;
    }

    public boolean seeLoad (String file)
    {
	// NYI...
	return false;
    }

    private MonkMatch cdes, ldes, ddes, sdes;

    /**
     * Generates an "empty" XSC file with given ETD name and package.
     * Used to create a new custom XSC file.
     * Includes only the standard header and methods.
     *
     * @param xscFile  full path of the XSC output file
     * @param etdName  name of the XSC root node
     * @param etdPack  the Java package for generated code
     * @throws IOException for XSC output problems
     * @throws FileNotFoundException when SSC input not found
     * @throws UnsupportedEncodingException if SSC encoding not supported
     */
    public void empty (String xscFile, String etdName, String etdPack)
	throws IOException, FileNotFoundException, UnsupportedEncodingException
    {
	// Write XSC file; delete old file, make directories as needed.
	File xsc = new File(xscFile);
	File xscDir = xsc.getParentFile();
	if (xscDir != null && ! xscDir.isDirectory())
	    xscDir.mkdirs();
	if (xsc.exists())
	    xsc.delete();

	// Create output stream.
	Writer writer =
	    new BufferedWriter(
		new OutputStreamWriter(
		    new FileOutputStream(xsc),
		    "UTF-8"));
	Emit e = new Emit(writer);

	// Write header, required root methods, and trailer.
	Xsc.JavaProps jprops = new Xsc.JavaProps(null, null, false, etdPack,
	    makeJavaId(etdName, new HashSet(), new HashSet(), asciiOnly),
	    null, null);
	xscHead(e, "SSC", false, 0, etdName, etdPack, null, null, null, null, jprops);
	e.emit("<class name=\"" + sscNormal(etdName) + "\" type=\"CLASS\"");
	e.indent();
	e.emit("structure=\"delim\""
	    + " order=\"sequence\" comment=\"root node\">");
	xscStdMethods(e, null, addThrows);
	e.down("</class>");
	xscTail(e);
	e.close();
    }

    private String sscEncoding = null;
    GenXsc genXsc = null;

    /**
     * Converts given global delimiter specification.
     * Outputs result to "e".
     *
     * @param d  delimitier specificiation
     * @param e  output emit buffer
     */
    private void convDelim (MonkData d, Emit e)
    {
	byte type = 0;
	Xsc.Delim delim = new Xsc.Delim(null, uid(), type);
	for (; d.isPair(); d = d.getCdr())
	{
	    MonkData del = d.getCar();

	    if (del.isSymbol())
	    {
		// The delimiter flags.
		String as = del.getSymbol();
		if (as.equals("anchored"))
		    type |= delim.ANCHORED;
		else if (as.equals("beginanchored"))
		    type |= delim.BEGINANCH;
		else if (as.equals("endanchored"))
		    type |= delim.ENDANCH;
		else if (as.equals("array"))
		    type |= delim.ARRAY;
		else if (as.equals("endofrec"))
		    type |= delim.ENDOFREC;
		else if (as.equals("required"))
		    type |= delim.REQUIRED;
		else if (as.equals("separator"))
		    type |= delim.SEPARATOR;
		else
		    die(del, "unconvertible modifier ["
			+ as + "]");
		delim.setFlags(type);
	    }
	    else if (del.isString())
	    {
		// Old-style simple delimiter: single end-delimiter.
		if (((MonkString) del).length < 1)
		    die(del, "delimiter can't be empty string");
		Xsc.DelimGroup group =
		    new Xsc.DelimGroup(null, uid(), null,
			makeOneDelim(del));
		delim.addGroup(group);
	    }
	    else if (del.isInteger())
	    {
		// Offset/length encoded delimiter.
		long offset = ((MonkInteger) del).value;
		long length = 1;
		if (d.getCdr() != null &&
		    d.getCdr().isPair() &&
		    d.getCdr().getCar().isInteger())
		{
		    // Consume 2nd argument: length.
		    length = ((MonkInteger)
			d.getCdr().getCar()).value;
		    d = d.getCdr();
		}
		if (offset < 0 || offset > Integer.MAX_VALUE)
		    die(del, "delimiter offset value negative "
			+ "or too large");
		if (length < 1 || length > Integer.MAX_VALUE)
		    die(del, "delimiter length value negative, "
			+ "zero or too large");
		Xsc.DelimGroup group =
		    new Xsc.DelimGroup(null, uid(), null,
			new Xsc.OneDelim(null, uid(),
			    (int) length, (int) offset));
		delim.addGroup(group);
	    }
	    else if (del.isPair())
	    {
		// Sublist.
		MonkData part = del.getCar();
		if (part == null)
		    die(del, "empty sublist in delimiter");
		if (part.isString())
		{
		    // Simple begin/end delimiter pair
		    MonkData bd = part;
		    MonkData ed = del.getCdr().getCar();
		    if (bd != null && bd.isString() &&
			ed != null && ed.isString())
		    {
			if (((MonkString) bd).length < 1 ||
			    ((MonkString) ed).length < 1)
			{
			    die(del, "delimiter can't be empty string");
			}
			Xsc.DelimGroup group =
			    new Xsc.DelimGroup(null, uid(),
				makeOneDelim(bd),
				makeOneDelim(ed));
			delim.addGroup(group);
		    }
		    else
		    {
			die(del, "malformed delimiter pair; "
			    + "expected two strings, got "
			    + (bd == null ? "nothing" : bd.typeName())
			    + " and "
			    + (ed == null ? "nothing" : ed.typeName()));
		    }
		}
		else if (part.isSymbol())
		{
		  if (part.isSymbol("EscD")) {
		    // escape delimiter list.
		    for (del = del.getCdr(); del.isPair(); del = del.getCdr())
		      {
			part = del.getCar();
			if (part.isString())
			  {
			    if (((MonkString) part).length < 1)
			      die(part, "escape delimiter can't be empty string");
			    delim.addEscape(makeEscape(part));
			  }
			else
			  {
			    die(del, "escape delim sublist part can't be "
				+ part.typeName());
			  }
		      }
		  }
		  else {
		    Xsc.OneDelim bd = null;
		    Xsc.OneDelim ed = null;
		    Xsc.DelimGroup group = new Xsc.DelimGroup(null, uid(),
							      null, null);
		    delim.addGroup(group);
		    boolean begin = true;
		    // Mutiple/paired delimiter list.
		    for (; del.isPair(); del = del.getCdr())
		      {
			part = del.getCar();
			if (part.isSymbol("Bd"))
			  {
			    // Start of begin-delimiters. */
			    begin = true;
			  }
			else if (part.isSymbol("Ed"))
			  {
			    // Start of end-delimiters. */
			    begin = false;
			  }
			else if (part.isString())
			  {
			    if (((MonkString) part).length < 1)
			      die(part, "delimiter can't be empty string");
			    if (begin)
			      group.addBegin(makeOneDelim(part));
			    else
			      group.addEnd(makeOneDelim(part));
			  }
			else
			  {
			    die(del, "sublist part can't be "
				+ part.typeName());
			  }
		      }
		  }
		}
		else
		{
		    die(del, "sublist start can't be " + part.typeName());
		}
	    }
	    else
	    {
		// Huh?
		die(del, "modifier can't be " + del.typeName());
	    }
	}
	delim.setFlags(type);
	genXsc.genDelim(delim);
	delim = null;
    }

    /**
     * Reads the given SSC file, converts it to the given XSC output file.
     * Handles alternate encoding.
     *
     * @param ssc  SSC input file name (below input root)
     * @param etdPack  package name
     * @param sx  output: SSC to XSC mapping list (lines)
     * @param extern  output: list of loaded files not in sscs[]
     */
    public void convertOne (String ssc, File xsc, String etdPack,
	StringBuffer sx, HashSet extern)
	throws IOException
    {
	//if (debug)
	    System.out.println("[ convertOne <" + ssc + "> to <"
		+ xsc.getPath() + ">, package=<" + etdPack + "> ]");

	// Put XSC file into output tree.
	File xscDir = xsc.getParentFile();
	if (xscDir != null && ! xscDir.isDirectory())
	    xscDir.mkdirs();
	if (xsc.exists())
	    xsc.delete();
	Writer fw =
	    new BufferedWriter(
		new OutputStreamWriter(
		    new FileOutputStream(xsc),
		    "UTF-8"));
	StringWriter sw = new StringWriter();
	Emit e = new Emit(sw);
	if (genXsc == null)
	    genXsc = new GenXsc();
	genXsc.e = e;

	sscEncoding = null;
	String etdName = null;
	e.indent();

	// Iterate through file top-level elements.
	MonkLexer in = new MonkLexer((new File(sscInputRoot, ssc)).getPath());
	while (true)
	{
	    MonkData m = in.next();
	    if (m.type == MonkData.SC_EOF)
		break;
	    if (! m.isPair())
		die(m, "unexpected " + m.typeName() + " at top-level");

	    // Strip leading keyword.
	    MonkPair mp = (MonkPair) m;
	    if (! mp.car.isSymbol())
		die(m, "expected list to start with keyword, not "
		    + mp.car.typeName());
	    String key = mp.car.getSymbol();

	    // We have "(<key> ...)".
	    if (key.equals("set-file-encoding-method"))
	    {
		/* Change of encoding.  Interpret this by switching
		 * the input lexer to a new charset encoding.
		 */
		if (cdes == null)
		    cdes = new MonkMatch("encoding clause", "-YY)", 1, null);
		cdes.match(true, m, null);
		sscEncoding = cdes.out(0).getSymbol();
		if (sscEncoding.startsWith(":"))
		    sscEncoding = sscEncoding.substring(1);
		in.setEncoding(sscEncoding);
	    }
	    else if (key.equals("load"))
	    {
		/* External template declaration.  Look it up here once.
		 * Note that a load does not necessarily mean the template
		 * will be used by references in the event structure.
		 * SSC files contains a "load" statement for each external
		 * template. Note that they follow any set-file-encoding-method
		 * calls in the SSC files generated by the Monk ETD editor,
		 * but we ignore the encoding for now...
		 */
		if (ldes == null)
		    ldes = new MonkMatch("load statement", "-YS)", 1, null);
		ldes.match(true, m, null);
		String rawName = ldes.out(0).getString();
		if (rawName == null) { die(m, "load-stmt f**ked"); }
		String relName = normaliseLoad(rawName);
		if (debug)
		    System.out.println("[ - search <" + rawName + "> ]");
		if (relName == null)
		    throw new RuntimeException("cannot resolve \"" + rawName
			+ "\" reference in \"" + ssc + "\"");
		/* Found file, now map directory.  Note that the call to
		 * xscPathUnique() has the required side-effect of assuring
		 * the file-name mapping is unambiguous.
		 */
		if (debug)
		    System.out.println("[ - found as <" + relName + "> ]");
		File relFile = new File(relName);
		extern.add(Ssc2xsc.slashPath(relFile));
		String sscDir = relFile.getParent();
		String xscName =
		    ((sscDir == null) ? "" : (xscDirUnique(sscDir)))
		    + relFile.getName();
		if (debug)
		    System.out.println("[ - output as <" + relName + "> ]");
		sx.append("LOAD " + ssc + " " + Ssc2xsc.slashPath(relFile)
		    + " " + xscName + "\n");
	    }
	    else if (key.equals("define"))
	    {
		// Delimiter list, or template, else unknown.
		if (debug)
		    System.out.println("[ seen define ]");
		if (ddes == null)
		    ddes = new MonkMatch("define clause", "-YYX)", 2, null);
		ddes.match(true, m, null);
		MonkData sym = ddes.out(0);
		MonkData def = ddes.out(1);
		if (def.isPair() && def.getCar().isSymbol("quote"))
		{
		    /* Got a "(define <name>-delm (quote ...))" delimiter list.
		     * If the SSC file contains non-standard code using a
		     * define with a quoted argument, we will barf.
		     */
		    if (debug)
			System.out.println("[ seen delimiter list ]");
		    e.down("<delimiters uid=\"" + uid() + "\">");
		    for (MonkData d = def.getCdr().getCar();
			d.isPair(); d = d.getCdr())
			    convDelim(d.getCar(), e);
		    e.done("</delimiters>");
		    e.emit();
		}
		else if (sym.isSymbol() && def.isPair() &&
		    (def.getCar().isSymbol("message-convert") ||
		     def.getCar().isSymbol("$resolve-event-definition")) &&
		    def.getCdr().getCar().isPair() &&
		    def.getCdr().getCar().getCar().isSymbol("quote"))
		{
		    /* Here we have the start of a field list, with this syntax:
		     * (define NAME-struct (message-convert (quote FIELDS)))
		     * This must be either a local template definition or the
		     * top-level structure; they look the same.
		     * Note: "$resolve-event-definition" is the newer form.
		     */
		    if (debug)
			System.out.println("[ seen message struct ]");

		    // Check name, strip "-struct" suffix.
		    etdName = sym.getSymbol();
		    if (etdName.endsWith("-struct"))
			etdName = etdName.substring(0, etdName.length() - 7);
		    else
		    {
			/*-
			//Name does not end with "-struct" !?
			warn("warning: non-struct message name <"
			    + name + ">\n")
			-*/
		    }
		    crunchPackage = etdPack;
		    convField(e, def.getCdr().getCar().getCdr().getCar(),
			new HashSet(), new HashSet(), 0);
		}
	    }
	    else
	    {
		// Unexpected at top.  Maybe this should warn and skip...
		die(m, "unknown keyword \"" + key + "\"");
	    }
	}
	in.close();
	if (etdName == null)
	    throw new IOException("file \"" + ssc
		+ "\" does not contain a main node");

	// Prepend standard header, and insert standard method info.
	e.setOut(fw);
	e.redent();
	Xsc.JavaProps jprops = new Xsc.JavaProps(null, null, false, etdPack,
	    makeJavaId(etdName, new HashSet(), new HashSet(), asciiOnly),
	    null, null);
	xscHead(e, "SSC", false, 0, etdName, etdPack,
	    GenSsc.monk2javaCode(sscEncoding), null, null, null, jprops);
	String meth = sw.getBuffer().toString();
	int lastMark = meth.lastIndexOf(METHMARK);
	if (lastMark < 0)
	{
	    // Some kind of internal problem...
	    throw new RuntimeException("no method marker found");
	}
	// Copy 1st half, with mark elided.
	for (int prevMark = 0; prevMark < lastMark; )
	{
	    int methMark = meth.indexOf(METHMARK, prevMark);
	    e.emi0(meth.substring(prevMark, methMark));
	    prevMark = methMark + 1;
	}
	// Replace last mark by standard method info.
	e.indent();
	e.indent();
	xscStdMethods(e, null, addThrows);
	e.undent();
	e.undent();
	// Copy 2nd half.
	e.part(meth.substring(lastMark + 1));
	xscTail(e);
	e.close();
    }

    /**
     * Generates standard SSC-based XSC file header.
     * Leaves indentation level appropriate for content.
     *
     * @param e  the output stream
     * @param etdType  the type name
     * @param version  the code version (interface tag)
     * @param etdName  the name of the root node
     * @param etdPack  the package name
     * @param encoding  the "sscEncoding" attribute, or null
     * @param dataCode  the "dataEncoding" attribute, or null
     * @param etdComm  comment in &lt;etd&gt;, if any
     * @param etdUid  preferred UID in &lt;etd&gt;, if any
     * @param jprops  the &lt;<javaProps>&gt; info, if any
     */
    public void xscHead (Emit e, String etdType, boolean derived, int version,
	String etdName, String etdPack, String encoding, String dataCode,
	String etdComm, String etdUid, Xsc.JavaProps jprops)
    {
	if (etdUid == null) { etdUid = uid(); }
	e.redent();
	e.emit("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
	e.emit("<!-- Generated by SscConv, XSC version "
	    + (xsc03 ? "0.3" : "0.6") + " -->");
	e.emit();
	e.part("<etd name=\"" + sscNormal(etdName)
	    + "\" type=\"SSC\" xscVersion=\"0.6\"");
	if (derived)
	    e.part(" derived=\"true\"");
	if (version > 0)
	    e.part(" codeVersion=\"" + version + "\"");
	if (xsc03)
	    e.part(" packageName=\"" + etdPack + '"');
	if (encoding != null)
	    e.part(" sscEncoding=\"" + encoding + '"');
	if (dataCode != null)
	    e.part(" dataEncoding=\"" + dataCode + '"');
	if (etdComm != null)
	    e.part(" comment=\"" + sscNormal(etdComm) + '"');
	e.emit(" editable=\"true\" uid=\"" + etdUid + "\">");
	e.indent();
	if (jprops != null)
	{
	    GenXsc genXsc = new GenXsc();
	    genXsc.e = e;
	    genXsc.addUid = false;
	    genXsc.genJavaProps(jprops);
	    e.emit();
	}
    }

    /**
     * Generates standard trailer.
     * Expects indentation level appropriate for content.
     *
     * @param e  XSC output stream
     */
    public void xscTail (Emit e)
    {
	e.done("</etd>");
    }

    /**
     * Generates the "standard" methods.
     *
     * @param e  XSC output stream
     * @param sscFile  the context for generating UIDs
     * @param doThrows  flag: emit &lt;throws&gt; information?
     */
    public void xscStdMethods (Emit e, SSC_File sscFile, boolean doThrows)
    {
	XscMethod m = new XscMethod(e, sscFile, doThrows);
	m.method("reset", "boolean", JCSConstants.resetMethodComment);

	m.method("available", "boolean", JCSConstants.availableMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment);
	m.method("next", "boolean", JCSConstants.nextMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment);
	m.method("receive", "boolean", JCSConstants.receiveMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment);
	m.param("destination", "java.lang.String",
	    JCSConstants.topicNameParamComment);
	m.method("receive", "boolean", JCSConstants.receiveMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment2);
	m.method("send", "void", JCSConstants.sendMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment2);
	m.param("destination", "java.lang.String",
	    JCSConstants.topicNameParamComment);
	m.method("send", "void", JCSConstants.sendMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment3);
	m.method("rawInput", "byte[]", JCSConstants.rawInputMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment3);
	m.method("topic", "java.lang.String", JCSConstants.topicMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment3);
	m.method("publications", "java.util.Vector",
	    JCSConstants.publicationsMethodComment);

	m.exception(JCSConstants.collabDataExceptionClass,
	    JCSConstants.collabDataExceptionComment3);
	m.method("subscriptions", "java.util.Vector",
	    JCSConstants.subscriptionsMethodComment);

	m.exception(JCSConstants.marshalExceptionClass,
	    JCSConstants.marshalExceptionComment);
	m.method("marshal", "byte[]", JCSConstants.marshalMethodComment);

	m.exception(JCSConstants.unmarshalExceptionClass,
	    JCSConstants.unmarshalExceptionComment);
	m.param("blob", "byte[]", JCSConstants.blobParamComment);
	m.method("unmarshal", "void", JCSConstants.unmarshalMethodComment);

	m.param("propName", "java.lang.String",
	    JCSConstants.readPropNameParamComment);
	m.method("readProperty", "java.lang.String",
	    JCSConstants.readPropertyMethodComment);

	m.param("propName", "java.lang.String",
	    JCSConstants.writePropNameParamComment);
	m.param("propValue", "java.lang.String",
	    JCSConstants.writePropValueParamComment);
	m.method("writeProperty", "void",
	    JCSConstants.writePropertyMethodComment);
    }

    static final HashSet javaKeyword = new HashSet();
    static
    {
	// Initialize "javaKeyword".
	String[] key =
	{
	    // Current and future keywords + primitive types.
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
	    "var", "void", "volatile", "while", "widefp",

	    // Standard classes.
	    "Boolean", "Byte", "Character", "Double", "Float",
	    "Integer", "Long", "Short", "String",

	    // Conflict with Object and ETD: getClass, getMode, hasData.
	    "Class", "Mode", "Data"
	};
	for (int i = 0; i < key.length; i ++)
	    javaKeyword.add(key[i]);
	    
	    
	   // ESR 104056   
     // check here for the javakeyword from 
     // the jcsrc file
     // if we are to change the javaKeyword we modified it from 
     // being a static final to a simple static var
     // add the SscConv.noKeyword to the .jcsrc file for this 
     // to work for SWIFT only
    
    final String sData_key = JCSProperties.getProperty("SscConv.noKeyword") ;

    if (sData_key != null) {
     // debug
     //System.out.println("sData_key .. " + sData_key  );
     StringTokenizer sResult = new StringTokenizer(sData_key, ",");
     // debug
     //System.out.println("sResult for Swift stuff is .. " + sResult.countTokens()  );
      while(sResult.hasMoreElements())
      {
    	 javaKeyword.remove(sResult.nextToken());
      }
    }
	    
    }// end of static block

    /**
     * Tests if the given string is a Java reserved keyword.
     *
     * @return thetest result
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
     */
    private String xscDirPathTranslate (String dir)
    {
	if (dir.equals("."))
	    return "";

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
	    else if (! ('a' <= c && c <= 'z') && c != '/')
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
	if (debug)
	    System.out.println("[ ssc dir <" + dir + "> = xsc dir <"
		+ b.toString() + "> ]");
	return b.toString();
    }

    private Hashtable s2xMap = new Hashtable(); // cache of SSC->XSC directories
    private Hashtable x2sMap = new Hashtable(); // cache of XSC->SSC directories
    private StringBuffer monkLibs = new StringBuffer();

    /**
     * Converts relative directory path to path useful for Java package.
     * Like xscDirPathTranslate(), but caches the results, and makes sure
     * that two different input paths do not map to the same normalised
     * output directory path.
     *
     * @param sscDir  an SSC directory name
     * @return the XSC directory name
     */
    private String xscDirUnique (String sscDir)
    {
	// Canonize separators.
	sscDir = sscDir.replace('\\', '/');
	if (sscDir.endsWith("/"))
	    sscDir = sscDir.substring(0, sscDir.length() - 1);

	String xscDir = (String) s2xMap.get(sscDir);
	if (xscDir == null)
	{
	    xscDir = xscDirPathTranslate(sscDir);
	    String copy = (String) x2sMap.get(xscDir);
	    if (copy != null)
		throw new RuntimeException("SSC dirs " + copy + " and \""
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
     * @param prefix  package prefix, should be empty or end in "/"
     * @param dir  relative path of directory
     * @return a Java package name
     */
    static String xscPackage (String prefix, String dir)
    {
	if (dir == null) { dir = ""; }
	String result = prefix + dir.replace('/', '.');
	return result.endsWith(".")
	    ? result.substring(0, result.length() - 1)
	    : result;
    }

    /**
     * This function translates a tree of SSC files to a tree of XSC files.
     * The input tree will mirror the output tree, so that each input SSC
     * file corresponds to one output XSC file, and the relative path
     * between two input files will be the same as the relative path between
     * the two corresponding output files, after taking into account directory
     * name translation.  An input directory name may be translated to a
     * different output directory name, to make it acceptable as a Java class
     * directory name; for example, "foo.bar" might become "fooBar".
     * The base-name of an input and its output file will remain the same,
     * but the suffix changes from ".ssc" to ".xsc".
     * [Note that the base-name does *not* have to be acceptable as a Java
     * class-name; cf. the &lt;javaProps&gt; "class" attribute.]
     * <p>
     * The input to the function is:
     *
     * (1) The root of the input tree.
     *     All further input file identification must be in/below this
     *     directory.
     *
     * (2) The root of the output tree.
     *     All generated output SSC files will be in or below this directory.
     *     The function will create any subdirectories if (and only if) needed.
     *
     * (3) The Monk load-path list.
     *     This is an (optionally empty) ordered list of directories, relative
     *     to the input root, that external template references in the SSC
     *     file could be relative to (rather like "-I" for a C compiler).
     *     If any of the files to be converted contains external references,
     *     then the reference *must* be resolvable along the load-path,
     *     because the reference paths are normalised in the output.
     *
     * (4) The SSC file list.
     *     The SSC file list is a (possibly empty) unordered list of input
     *     files, all specified as paths relative to the input tree root.
     *
     * (5) The normalised path prefix.
     *     This represents the relative path from the root of the normalised
     *     external template references to the directory that the output tree
     *     root will be placed under at run-time.  The prefix is added before
     *     each normalised path.  A normalised path is produced from the
     *     prefix, plus the relative path of the referred file from the output
     *     tree root.
     *
     * (6) The package prefix.
     *     Must be empty or end with a ".".
     *     This represents the Java package that the output tree's corresponding
     *     Java classes will be placed under.  The package name is produced from
     *     the prefix, plus the relative path of the output file from the output
     *     tree root.
     * <p>
     * It is possible to resolve references to external template files without
     * translating those files themselves, useful for e.g. converting shared
     * templates separately from the SSC files that use those template.
     * The output of the function, aside from logs and errors, is:
     *
     * (1) The output tree of XSC files.
     *
     * (2) A list of input to output file correspondences.
     *
     * (3) A list of external references, each reference consisting of the
     *     relative name of the input file (below the input tree root), the
     *     relative name of the referred input file, and the name of what
     *     *would* be the name of the corresponding output path for the
     *     referred file (i.e. with translated output directory names).
     * <p>
     * The rationale for (3) is that this makes it easy to use it from higher
     * level scripts to do partial or incremental conversion of libraries of
     * SSC files, or to conditionally commit output files.
     * <p>
     * Each output XSC file will have a &lt;javaProps&gt; element with "class" and
     * "package" attributes. The "class" value will be the acceptable Java name
     * equivalent of the base-name of the file; the "package" will consist of
     * the package prefix followed by the relative path of the output file's
     * directory (with "." instead of "/" as a directory name separator).
     *
     * @param idir  input directory of SSC files
     * @param odir  output directory of XSC files
     * @param loads  Monk load-path, directories relative to idir
     * @param sscs  SSC files, relative to idir
     * @param prepack  package prefix: {NAME "."}
     * @param dumb  if non-null, dumb conversion prefix for references
     * @param tmpMonk  temporary file to use; null=make one up
     * @param tmpXsc  temporary file to use; null=make one up
     * @param uconv  convert non-ASCII to U-escape in names?
     * @param sx  output: SSC to XSC mapping list (lines)
     * @param extern  output: list of loaded files not in sscs[]
     *
     * @throws IOException for I/O problems
     * @throws SAXException for XSC syntax errors
     */
    public static void convert2 (File idir, File odir, String[] loads,
	String[] sscs, String prepack, String dumb,
	File tmpMonk, File tmpXsc, boolean uconv, StringBuffer sx,
	HashSet extern)
	throws IOException, SAXException
    {
	/*-
	if (debug)
	    System.out.println("[ trans <" + idir.getPath()
		+ "> to <" + odir.getPath() + ">, prepack=\""
		+ prepack + "\" ]");
	-*/

	// Sanity checks.
	if (sscs.length == 0)
	    return;
	if (idir == null)
	    throw new IOException("convert: no input directory");
	if (! idir.exists())
	    throw new IOException("convert: input directory \""
		+ idir.getPath() + "\" does not exist");
	if (odir == null)
	    throw new IOException("convert: no output directory");
	if (! odir.exists())
	    throw new IOException("convert: output directory \""
		+ odir.getPath() + "\" does not exist");

	SscConv sscConv = new SscConv(idir, loads, prepack);
	sscConv.asciiOnly = uconv;
	sscConv.dumbSscRoot = dumb;
	sscConv.trans(odir, sscs, tmpXsc, sx, extern);
    }

    private String prepack;	 // package prefix: {<name> "."}

    /**
     * Registers the common conversion context.
     */
    public SscConv
	(
	File idir,	 // input directory of SSC files
	String[] search, // Monk load-path, directories relative to idir
	String prepack	 // package prefix: {<name> "."}
	)
    {
	this.sscInputRoot = idir;
	this.sscSearch = search;
	this.prepack = (prepack == null ? "" : prepack);
    }

    /**
     * Translates the given list of SSC files, using the context provided
     * by the class instance for translation context.
     * Also adds a line for each load to "sx" for each load, with 3 space
     * separated fields: SSC file name, loaded file (resolved), and the
     * equivalent XSC file.
     *
     * @param odir  output directory of XSC files
     * @param sscs  array of SSC files, relative to idir
     * @param tmpXsc  temporary file to use; null=make one up
     * @param sx  output: SSC to XSC mapping list (lines)
     * @param extern  output: list of loaded files not in sscs[]
     *
     * @throws IOException for I/O problem
     * @throws FileNotFoundException for SSC files in list but not on disk
     * @throws UnsupportedEncodingException for encoding problems in SSC
     */
    public void trans (File odir, String[] sscs, File tmpXsc,
	StringBuffer sx, HashSet extern)
	throws IOException, FileNotFoundException, UnsupportedEncodingException
    {
	/* For each name sscs[i], generate the output filename xscs[i].
	 * Make sure this does not generate duplicate pathnames because of
	 * the translation of directories for Java package name compliance.
	 */
	String[] xscs = new String[sscs.length];
	for (int i = 0; i < sscs.length; i ++)
	{
	    File ssc = new File(sscs[i]);
	    String parent = ssc.getParent();
	    String sscDir = ((parent == null)
		? ""
		: (parent + "/").replace('\\', '/'));
	    String xscDir = xscDirUnique(sscDir);
	    String base = ssc.getName();
	    if (base.endsWith(".ssc"))
		base = base.substring(0, base.length() - 4);
	    xscs[i] = xscDir + base + ".xsc";
	    sx.append("CONV " + sscs[i] + " " + xscs[i] + "\n");
	}

	if (tmpXsc == null)
	{
	    tmpXsc = File.createTempFile("s2x-", ".xsc");
	    if (debug)
		System.out.println("[ - xsc <" + tmpXsc.getPath() + "> ]");
	}

	// Loop over the original SSC files.
	for (int i = 0; i < sscs.length; i ++)
	{
	    // Generate XSC text for SSC input.
	    String parent = (new File(xscs[i])).getParent();
	    if (parent != null)
		parent = parent.replace('\\', '/');
	    String etdPack = xscPackage(prepack, parent);
	    convertOne(sscs[i], new File(odir, xscs[i]), etdPack, sx, extern);
	}

	// Delete the original SSC files from the external set.
	for (int i = 0; i < sscs.length; i ++)
	    extern.remove(sscs[i]);
    }
}
