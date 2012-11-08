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
 * @(#)Monk.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.io.PrintStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Vector;

import com.sun.stc.jcs.ssc.MulticodeStreamReader;
import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.Option;

/**
 * Monk data manipulation in Java; used for SSC conversion.
 * Code to read and process Monk data.
 * Monk is SeeBeyond's extended subset of Scheme, used for
 * message handling and for general glue code.
 *
 * NYI: loc-info in error msgs
 */
public class Monk
{
    /**
     * Describes origin of an item of Monk data.
     * This is used in error messages to pinpoint the source of a problem.
     */
    public static class MonkLoc
    {
	private String file;
	private int line;

	/**
	 * Creates origin from given file and line number.
	 *
	 * @param file  name of file, or null  if unknown
	 * @param line  number of line in file, or 0 if unknown
	 */
	public MonkLoc (String file, int line)
	{
	    this.file = file;
	    this.line = line;
	}

	/**
	 * Create empty origin (nowhere).
	 */
	public MonkLoc ()
	{
	    this.file = null;
	    this.line = 0;
	}

	// Access.
	public String getFile () { return file; }
	public int getLine () { return line; }

    @Override
	public String toString ()
	{
	    return "(MonkLoc file: " + file + " line: " + line + ")";
	}
    };

    /**
     * The base class for an item of Monk data.
     * All other "primitive" Monk types are derived from this class.
     */
    public static class MonkData
    {
	// Normal Monk items.
	public static final byte NONE      = 000;
	public static final byte BOOLEAN   = 001;
	public static final byte CHARACTER = 002;
	public static final byte INTEGER   = 003;
	public static final byte LAMBDA    = 004;
	public static final byte NILVALUE  = 005;
	public static final byte PAIR      = 006;
	public static final byte REAL      = 007;
	public static final byte STRING    = 010;
	public static final byte SYMBOL    = 011;
	public static final byte VECTOR    = 012;
	public static final byte PATH      = 013;
	// Transient types, used during scanning.
	public static final byte SC_CLOSE  = 015;
	public static final byte SC_DOT    = 016;
	public static final byte SC_EOF    = 017;

	public byte type;
	public MonkLoc loc;

	/**
	 * Provides a printable string for a Monk data item type.
	 * This method must be adapted when a new primitive type is added,
	 * else it will throw a runtime exception.
	 */
	public static String typeName (byte type)
	{
	    switch (type)
	    {
	    case NONE:      return "<none>";
	    //
	    case BOOLEAN:   return "boolean";
	    case CHARACTER: return "character";
	    case INTEGER:   return "integer";
	    case LAMBDA:    return "lambda";
	    case NILVALUE:  return "nil";
	    case PAIR:      return "pair";
	    case PATH:      return "path";
	    case REAL:      return "real";
	    case STRING:    return "string";
	    case SYMBOL:    return "symbol";
	    case VECTOR:    return "vector";
	    //
	    case SC_CLOSE:  return "<close>";
	    case SC_DOT:    return "<dot>";
	    case SC_EOF:    return "<eof>";
	    }
	    throw new RuntimeException("MonkData: unimplemented type");
	}

	public String typeName ()
	{
	    return typeName(type);
	}

	/**
	 * Tests if this is an integer.
	 *
	 * @return the test result
	 */
	public boolean isInteger ()
	{
	    return type == INTEGER;
	}

	/**
	 * Tests if this is an integer, with the given value.
	 *
	 * @param i  the value to compare with
	 * @return the test result
	 */
	public boolean isInteger (int i)
	{
	    return type == INTEGER &&
		((MonkInteger) this).value == i;
	}

	/**
	 * Tests if this is a string.
	 *
	 * @return the test result
	 */
	public boolean isString ()
	{
	    return type == STRING;
	}

	/**
	 * Tests if this is a string, with the given value.
	 *
	 * @param s  the string value to compare with
	 * @return the test result
	 */
	public boolean isString (String s)
	{
	    if (type != STRING)
		return false;
	    MonkString ms = (MonkString) this;
	    if (ms.length != s.length())
		return false;
	    for (int i = 0; i < ms.length; i ++)
		if (ms.value[i] != s.charAt(i))
		    return false;
	    return true;
	}

	/**
	 * Get Unicode string value of this item, or null if not a symbol.
	 *
	 * @return the string, or null
	 */
	public String getString ()
	{
	    return (type == STRING)
		? new String(((MonkString) this).value)
		: null;
	}

	/**
	 * Tests if this is a symbol.
	 *
	 * @return the test result
	 */
	public boolean isSymbol ()
	{
	    return type == SYMBOL;
	}

	/**
	 * Tests if this is a symbol, with the given name.
	 *
	 * @param s  the name to compare with
	 * @return the test result
	 */
	public boolean isSymbol (String s)
	{
	    return type == SYMBOL &&
		((MonkSymbol) this).value.equals(s);
	}

	/**
	 * Get name string of symbol, or null if this item is not a symbol.
	 *
	 * @return the name, or null
	 */
	public String getSymbol ()
	{
	    return (type == SYMBOL ? ((MonkSymbol) this).value : null);
	}

	/**
	 * Test if this is a nil-value.
	 *
	 * @return the test result
	 */
	public boolean isNil ()
	{
	    return type == NILVALUE;
	}

	/**
	 * Test if this is a dotted pair.
	 *
	 * @return the test result
	 */
	public boolean isPair ()
	{
	    return type == PAIR;
	}

	private static MonkNil nil = null;

	/**
	 * Gets CAR of pair, else return generic nil.
	 *
	 * @return the "contents of address register", or nil
	 */
	public MonkData getCar ()
	{
	    if (type == PAIR)
		return ((MonkPair) this).car;
	    if (nil == null)
		nil = new MonkNil();
	    return nil;
	}

	/**
	 * Gets CDR of pair, else return generic nil.
	 *
	 * @return the "contents of decrement register", or nil
	 */
	public MonkData getCdr ()
	{
	    if (type == PAIR)
		return ((MonkPair) this).cdr;
	    if (nil == null)
		nil = new MonkNil();
	    return nil;
	}

    @Override
	public String toString ()
	{
	    return "(MonkData type: "+ typeName() + " loc: " + loc + ")";
	}
    };

    /**
     * Class "MonkNumber" is the base class for all numeric types.
     */
    static abstract class MonkNumber extends MonkData
    {
    public boolean exact;

    @Override
	public String toString ()
	{
	    return "(MonkNumber exact: " + exact +" "+ super.toString() +")";
	}
    };

    /**
     * Special transient values for the Monk scanner.
     */
    public static class MonkSpecial extends MonkData
    {
	/**
	 * Constructs from a location and a Monk type.
	 * The type should be one of the special types: SC_CLOSE,
	 * SC_DOT, SC_EOF.  We don't bother to check here.
	 *
	 * @param loc  origin information
	 * @param type  one of SC_CLOSE, SC_DOT, SC_EOF
	 */
	public MonkSpecial (MonkLoc loc, byte type)
	{
	    this.type = type;
	    this.loc = loc;
	}

    @Override
	public String toString ()
	{
	    return "(MonkSpecial " + super.toString() + ")";
	}
    }

    /**
     * Monk's "boolean" type.
     */
    public static class MonkBoolean extends MonkData
    {
	public boolean value;

	/**
	 * Constructs from a location and a boolean value.
	 *
	 * @param loc  origin information
	 * @param value  the false/true value
	 */
	public MonkBoolean (MonkLoc loc, boolean value)
	{
	    this.type = MonkData.BOOLEAN;
	    this.loc = loc;
	    this.value = value;
	}

    @Override
	public String toString ()
	{
	    return "(MonkBoolean value: " + value + " " + super.toString() +")";
	}
    }

    /**
     * Monk's "character" type; can be up to 32 bits wide.
     */
    public static class MonkCharacter extends MonkData
    {
	public int value;

	/**
	 * Constructs from a location and an integer value.
	 *
	 * @param loc  origin information
	 * @param value  the character code (normally in range 0000-FFFF)
	 */
	public MonkCharacter (MonkLoc loc, int value)
	{
	    this.type = MonkData.CHARACTER;
	    this.loc = loc;
	    this.value = value;
	}

    @Override
	public String toString ()
	{
	    return "(MonkCharacter value: " + value +" "+ super.toString() +")";
	}
    }

    /**
     * Monk's "string" type.
     * Note that Monk strings are mutable in general, and may have various
     * widths and encodings.  In here, we assume all strings have already
     * been converted to Unicode.
     * <p>
     * In these string objects, we optionally store an encoding ID and a
     * byte array containing the string value according to the encoding;
     * This is mainly to support Monk lexical analysis of SSC files.
     */
    public static class MonkString extends MonkData
    {
    public char[] value;
    public int length;
    public byte[] bytes = null; // source code representation, before decoding
    public String code = null;  // Monk encoding, e.g. "SJIS"

	/**
	 * Creates a Monk string for given length, containing only
	 * the given chracter, and no specific encoding.
	 *
	 * @param loc  origin information
	 * @param length  string length
	 * @param init  the filler character
	 */
	public MonkString (MonkLoc loc, int length, char init)
	{
	    this.type = MonkData.STRING;
	    this.loc = loc;
	    this.length = length;
	    this.value = (length > 0 ? new char[length] : null);

	    // Initialize all chars.
	    while (length -- > 0)
		this.value[length] = init;
	}

	/**
	 * Creates a Monk string from both the original encoded byte stream
	 * and the decoded string.  This method does not check the correctness
	 * of the encoding, and does no encoding or decoding itself.
	 *
	 * @param loc  origin information
	 * @param sb  the decoded string
	 * @param bytes  the encoded byte stream
	 * @param code  the encoding used here
	 */
	public MonkString (MonkLoc loc, StringBuffer sb, byte[] bytes,
	    String code)
	{
	    this.type = MonkData.STRING;
	    this.loc = loc;
	    this.length = sb.length();
	    this.value = (this.length > 0 ? new char[length] : null);
	    this.bytes = bytes;
	    this.code = code;

	    // Initialize all chars.
	    for (int i = 0; i < this.length; i ++)
		this.value[i] = sb.charAt(i);
	}

	/**
	 * Creates a Monk string from a Java string.  This creates a string
	 * object without an associated encoded byte stream.
	 *
	 * @param loc  origin information
	 * @param sb  the Java string data
	 */
	public MonkString (MonkLoc loc, StringBuffer sb)
	{
	    this.type = MonkData.STRING;
	    this.loc = loc;
	    this.length = sb.length();
	    this.value = (this.length > 0 ? new char[length] : null);

	    // Initialize all chars.
	    for (int i = 0; i < this.length; i ++)
		this.value[i] = sb.charAt(i);
	}

    @Override
	public String toString ()
	{
	    return "(MonkString value: \"" + new String(value) + "\""
		+ " length: " + length
		//+ " bytes: [" + MonkUtils.asHex(bytes, 10) + "]"
		+ " code: " + code
		+ " " + super.toString() +")"
		;
	}
    }

    /**
     * Monk's "symbol" type; immutable interned strings.
     * In here, we assume all names have already been converted to Unicode.
     */
    public static class MonkSymbol extends MonkData
    {
    public String value;
    public int length;

	/**
	 * Creates from a location and the symbol name.
	 *
	 * @param loc  origin information
	 * @param value  the Unicode name string of the symbol
	 */
	public MonkSymbol (MonkLoc loc, String value)
	{
	    this.type = MonkData.SYMBOL;
	    this.loc = loc;
	    this.value = value.intern();
	}

    @Override
	public String toString ()
	{
	    return "(MonkSymbol value: " + value + " length: " + length
		+ " " + super.toString() + ")";
	}
    }

    /**
     * Monk's "path" type; not furher interpreted here.
     */
    public static class MonkPath extends MonkData
    {
    public String value;
    public int length;

	/**
	 * Creates from a location and the unparsed path expression.
	 *
	 * @param loc  origin information
	 * @param value  the Unicode string for the path expression
	 */
	public MonkPath (MonkLoc loc, String value)
	{
	    this.type = MonkData.PATH;
	    this.loc = loc;
	}

    @Override
	public String toString ()
	{
	    return "(MonkSymbol value: " + value + " length: " + length
		+ " " + super.toString() +")";
	}
    }

    /**
     * Monk's "vector" type, a mutable array of fixed length.
     */
    public static class MonkVector extends MonkData
    {
    public MonkData[] value;
    public int length;

	/**
	 * Creates from a location and the vector size.
	 * All elements are initially set to null (not Monk's nil).
	 *
	 * @param loc  origin information
	 * @param length  the number of elements in the vector
	 */
	public MonkVector (MonkLoc loc, int length)
	{
	    this.type = MonkData.VECTOR;
	    this.loc = loc;
	    this.length = length;
	    this.value = (length > 0 ? new MonkData[length] : null);

	    // Initialize to nil-values.
	    while (length -- > 0)
		this.value[length] = null;
	}

    @Override
	public String toString ()
	{
	    StringBuffer buf = new StringBuffer("(MonkVector length: "
		+ length + " value: [");
	    for(int ii=0; ii<this.length ;ii++) {
		MonkData data = value[ii];
		if(null == data)
		    buf.append("null, ");
		else
		    buf.append(data.toString()+", ");
	    }
	    buf.append("] " + super.toString() +")");
	    return buf.toString();
	}
    }

    /**
     * Monk's "nil" value.  It only serves to carry origin infomation.
     * In Monk, a nil CDR value in a dotted pair marks the end of a list.
     */
    public static class MonkNil extends MonkData
    {
	/**
	 * Creates without origin information.
	 * Included for completeness.
	 */
	public MonkNil ()
	{
	    this.type = MonkData.NILVALUE;
	    this.loc = new MonkLoc();
	}

	/**
	 * Creates from origin information.
	 *
	 * @param loc  origin information
	 */
	public MonkNil (MonkLoc loc)
	{
	    this.type = MonkData.NILVALUE;
	    this.loc = loc;
	}

    @Override
	public String toString ()
	{
	    return "(MonkNil " + super.toString() +")";
	}
    }

    /**
     * Monk's "dotted pair" type, excluding "nil".
     */
    public static class MonkPair extends MonkData
    {
    public MonkData car, cdr;
    public int length;

	/**
	 * Creates from origin information and its twin values.
	 *
	 * @param loc  origin information
	 * @param car  1st value in pair
	 * @param cdr  2nd value in pair
	 */
	public MonkPair (MonkLoc loc, MonkData car, MonkData cdr)
	{
	    this.type = MonkData.PAIR;
	    this.loc = loc;
	    this.car = car;
	    this.cdr = cdr;
	}

    @Override
	public String toString ()
	{
	    return "(MonkPair car: " + car + " cdr: " + cdr + " "
		+ super.toString() + ")";
	}
    }

    /**
     * Monk's "integer" type.
     * Includes a fixed "exact" flag (see R4RS for details).
     */
    public static class MonkInteger extends MonkNumber
    {
    public long value;

	/**
	 * Creates from origin information, exactness flag and value.
	 *
	 * @param loc  origin information
	 * @param exact  flag: value exact?
	 * @param value  integer number value
	 */
	public MonkInteger (MonkLoc loc, boolean exact, long value)
	{
	    this.type = MonkData.INTEGER;
	    this.loc = loc;
	    this.exact = exact;
	    this.value = value;
	}

    @Override
	public String toString ()
	{
	    return "(MonkInteger value: " + value + " " + super.toString()
		+ ")";
	}
    }

    /**
     * Monk's "real" type.
     * Includes a fixed "exact" flag (see R4RS for details).
     */
    public static class MonkReal extends MonkNumber
    {
    public double value;

	/**
	 * Creates from origin information, exactness flag and value.
	 *
	 * @param loc  origin information
	 * @param exact  flag: value exact?
	 * @param value  floating-point number value
	 */
	public MonkReal (MonkLoc loc, boolean exact, double value)
	{
	    this.type = MonkData.REAL;
	    this.loc = loc;
	    this.exact = exact;
	    this.value = value;
	}

    @Override
	public String toString ()
	{
	    return "(MonkReal value: " + value + " " + super.toString() + ")";
	}
    }

    /**
     * Class to print a Monk data element, including loop detection.
     */
    public static class MonkPrinter
    {
	private PrintStream p;
	private HashSet done;
	private int dentFactor = 2;
	private int lineWidth = 80;

	/**
	 * Creates from a print-stream, which is where all printing will
	 * be done to.
	 *
	 * @param p  the output stream
	 */
	public MonkPrinter (PrintStream p)
	{
	    this.p = p;
	    this.done = new HashSet();
	}

	/**
	 * Makes N-char hex string of a non-negative integer value.
	 *
	 * @param value  the value to convert
	 * @param len  the number of hexadecimal digits to return
	 * @return the hexadecimal literal
	 */
	private String hexString (int value, int len)
	{
	    char[] buf = new char[len];
	    for (; len-- > 0; value /= 16)
		buf[len] = "0123456789ABCDEF".charAt(value % 16);
	    return new String(buf);
	}

	/**
	 * Emit newline + dent spaces to output.
	 *
	 * @param dent  the indentation level
	 */
	private void indent (int dent)
	{
	    p.println();
	    while (dent-- > 0) { p.print(' '); }
	}

	/**
	 * Prints out a Monk data structure recursively.
	 * Auxiliary for print().  Return true if newline emitted.
	 * Not a very pretty printer, but good enough for debugging output.
	 * Detects loops in the data structure by entering all items into
	 * a hashset, which is robust but slow on large structures.
	 *
	 * @param m  any Monk data item
	 * @param dent  indentation level (0=not indented)
	 * @return whether a newline was printed
	 */
	private boolean print2 (MonkData m, int dent)
	{
	    if (m == null)
	    {
		p.print("<null>");
		return false;
	    }
	    if (done.contains(m))
	    {
		// Duplicate reference, already printed.
		p.print("<ref>");
		return false;
	    }
	    done.add(m);

	    boolean result = false;
	    switch (m.type)
	    {
	    case MonkData.BOOLEAN:
		p.print(((MonkBoolean) m).value ? "#t" : "#f");
		break;

	    case MonkData.CHARACTER:
		int c = ((MonkCharacter) m).value;
		switch (c)
		{
		case '\n': p.print("#\\newline"); break;
		case '\t': p.print("#\\space"); break;
		case ' ':  p.print("#\\tab"); break;
		default:
		    if (0x20 <= c && c <= 0x7E)
			p.print((char) c);
		    else if (c < 0x100)
			p.print("#\\x" + hexString(c, 2));
		    else if (c < 0x100)
			p.print("#\\u" + hexString(c, 4));
		}
		break;

	    case MonkData.STRING:
		p.print('"');
		MonkString ms = (MonkString) m;
		for (int i = 0; i < ms.length; i ++)
		{
		    int ch = (ms.value[i] & 0xFFFF);
		    switch (ch)
		    {
		    case '\b': p.print("\\b"); break;
		    case '\n': p.print("\\n"); break;
		    case '\r': p.print("\\r"); break;
		    case '\t': p.print("\\t"); break;
		    case '\"': p.print("\\\""); break;
		    case '\\': p.print("\\\\"); break;
		    //NYI: avoid double backslash before [, ], {, }...
		    default:
			if (0x20 <= ch && ch <= 0x7E)
			    p.print((char) ch);
			else if (ch < 0x100)
			    p.print("\\x" + hexString(ch, 2));
			else if (ch < 0x100)
			    p.print("\\u" + hexString(ch, 4));
		    }
		}
		p.print('"');
		break;

	    case MonkData.INTEGER:
		MonkInteger mi = (MonkInteger) m;
		if (! mi.exact)
		    p.print("#i");
		p.print(mi.value);
		break;
	    case MonkData.REAL:
		MonkReal mr = (MonkReal) m;
		if (! mr.exact)
		    p.print("#i");
		p.print(mr.value);
		break;

	    case MonkData.SYMBOL:
		p.print(((MonkSymbol) m).value);
		break;
	    case MonkData.PATH:
		p.print(((MonkPath) m).value);
		break;

	    case MonkData.NILVALUE:
		p.print("()");
		break;

	    case MonkData.PAIR:
		p.print("(");
		dent += dentFactor;
		boolean dented1 = false;
		MonkPair mp = (MonkPair) m;
		for (int i = 0; true; i ++)
		{
		    if (dented1 || mp.car.type == MonkData.PAIR)
			{ indent(dent); result = true; }
		    else if (i > 0)
			p.print(" ");
		    dented1 = print2(mp.car, dent);
		    result |= dented1;
		    if (mp.cdr.type == MonkData.NILVALUE)
			break;
		    if (mp.cdr.type != MonkData.PAIR)
		    {
			p.print(" . ");
			result |= print2(mp.cdr, dent);
			break;
		    }
		    mp = (MonkPair) mp.cdr;
		    if (done.contains(mp))
		    {
			// Duplicate reference, already printed.
			p.print(". <ref>");
			break;
		    }
		    done.add(mp);
		}
		dent -= dentFactor;
		if (result)
		    indent(dent);
		p.print(")");
		return result;

	    case MonkData.VECTOR:
		p.print("(");
		dent += dentFactor;
		boolean dented2 = false;
		MonkVector mv = (MonkVector) m;
		p.print("#(");
		for (int i = 0; i < mv.length; i ++)
		{
		    if (dented2 || mv.type == MonkData.PAIR)
		    {
			indent(dent);
			result = true;
		    }
		    else if (i > 0)
			p.print(" ");
		    dented2 = print2(mv.value[i], dent + dentFactor);
		    result |= dented2;
		}
		if (result)
		    indent(dent);
		p.print(")");
		break;

	    case MonkData.LAMBDA:
		p.print("<lambda>");
		break;

	    default:
		// Huh?
		p.print("<type=" + (int) m.type + "?>");
	    }
	    return result;
	}

	/**
	 * Print item to output.
	 *
	 * @param m  any Monk data item
	 */
	public void print (MonkData m)
	{
	    print2(m, 0);
	    p.println();
	    p.flush();
	}
    }

    /**
     * Class to hold a Monk lexical scanner for a single source file/string.
     */
    public static class MonkLexer
    {
	private String file = null;
	private int line = 0; // current line
	private static int seqnoAll = 0;
	private int seqno = ++ seqnoAll;
	private String code = "ASCII"; // current encoding
	public boolean debug = false;

	static MonkNil nil = new MonkNil();
	static byte monkId[] = new byte[0x10000];

	static final byte IDC1 = 1; // bit-mask for 1st char Monk-ID
	static final byte IDCH = 2; // bit-mask for 2nd and rest of Monk-ID
	static final byte PATH = 4; // bit-mask for chars in Monk path

	static
	{
	    // Register the valid chars for Monk symbols in the scanner.
	    for (char c = 0x0000; c <= 0xFFFD; c ++)
			 monkId[c] = (byte)(
		    (Character.isLetter(c) ? IDC1 : 0) +
		    (Character.isLetterOrDigit(c) ? (IDCH+PATH) : 0));

	    // Chars valid as first char of a Monk name.
	    mSet(IDC1, "-.,:!?^$&%_=+*/<>|\uFF10-\uFF19");
	    mSet(IDC1, JCSProperties.getProperty("Monk.idStart"));

	    // Chars valid as 2nd and later char of a Monk name.
	    mSet(IDCH, "-.,:!?^$&%_=+*/<>|@~\u30fb"
		   + "\u3000-\u3002" + "\uFF07-\uFF0A"
		   + "\uFF1A-\uFF20" + "\uFF3B-\uFF40"
		   + "\uFF5B-\uFF5E" + "\uFF61-\uFF65");
	    mSet(IDCH, JCSProperties.getProperty("Monk.idPart"));

	    // Chars valid in a Monk path expression after the "~".
	    mSet(PATH, "-.,:!?^$&%_=+*/<>|[]");
	    mSet(PATH, JCSProperties.getProperty("Monk.idPath"));
	}

	/**
	 * Registers given range of chars as (not) having given property.
	 *
	 * @param chars  string of chars to set; a-b means range
	 * @param flags  bits to set
	 */
	private static void mSet (byte flags, String chars)
	{
	    if (chars == null) { return; }
	    int len = chars.length();
	    for (int i = 0; i < len; i ++)
	    {
		char lo = chars.charAt(i);
		char hi = (i+2 < len && chars.charAt(i+1) == '-')
		    ? chars.charAt(i+2) : lo;
		for (; lo <= hi; lo ++) { monkId[lo] |= flags; }
	    }
	}

	private MulticodeStreamReader in;
	private static final int bufSize = 2048;

	/**
	 * Creates lexer to scan some Monk source from an input stream.
	 * The "loc" parameter provides the initial position, which will
	 * be updated by the lexer during scanning.
	 *
	 * @param loc  the position object, initialized to the start position
	 * @param in  the input stream
	 */
	public MonkLexer (MonkLoc loc, MulticodeStreamReader in)
	{
	    this.file = loc.getFile();
	    this.line = loc.getLine();
	    this.in = in;
	}

	/**
	 * Creates lexer to scan some Monk source from a byte array.
	 *
	 * @param intext  the encoded Monk source text
	 * @throws IOException for I/O problems or syntax errors
	 */
	public MonkLexer (byte[] intext)
	    throws IOException
	{
	    this.file = null;
	    this.line = 1;
	    this.in = new MulticodeStreamReader(
		new ByteArraySeekInputStream(intext), bufSize);
	}

	/**
	 * Creates lexer to scan some Monk source from a file.
	 *
	 * @param infile  the file path
	 * @throws IOException for I/O problems or syntax errors
	 */
	public MonkLexer (String infile)
	    throws IOException
	{
	    if (debug) System.out.println("[ monk-lexer <" + infile + "> ]");
	    this.file = infile;
	    this.line = 1;
	    this.in = new MulticodeStreamReader(
		new FileSeekInputStream(infile), bufSize);
	}

	// Previous here() result.
	private String  hereFile = null;
	private int     hereLine = 0;
	private MonkLoc herePrev = null;

	/**
	 * Gets the current position.
	 * Caches the last value to share between succession of tokens
	 * on the same input line.
	 *
	 * @return the position as a MonkLoc value
	 */
	private MonkLoc here ()
	{
	    if (herePrev != null && hereLine == line &&
		(hereFile == null ? (file == null) : hereFile.equals(file)))
	    {
		// Same as previous.
		return herePrev;
	    }
	    return herePrev = new MonkLoc(hereFile = file, hereLine = line);
	}

	/**
	 * Provides a displayable character literal for a Unicode character.
	 * This shows a non-ASCII character as "U+xxxx".
	 *
	 * @param i  the character value
	 * @return the literal string
	 */
	static String charStr (int i)
	{
	    if (0x20 <= i && i < 0x7e)
		return ("'" + (char) i + "'");
	    String s = java.lang.Integer.toHexString(i + 0x10000);
	    return "U+" + s.substring(s.length() - 4, s.length()).toUpperCase();
	}

	/**
	 * Emits error message, noting the current source position.
	 *
	 * @param msg  the error message
	 * @throws IOException always thrown, with the message plus location
	 */
	public void die (String msg)
	    throws IOException
	{
	    StringBuffer eb = new StringBuffer();
	    if (file != null)
	    {
		eb.append("file \"");
		eb.append(file);
		eb.append("\": ");
	    }
	    if (line != 0)
	    {
		eb.append("line ");
		eb.append(line);
		eb.append(": ");
	    }
	    eb.append(msg);
	    throw new IOException(eb.toString());
	}

	/**
	 * Reads an N-digit octal integer literal from the input.
	 *
	 * @param len  the number of digits expected
	 @ @return the integer value
	 @ @throws IOException for input stream errors, or invalid digits
	 */
	public int getOct (int len)
	    throws IOException
	{
	    int result = 0, c;

	    while (len-- > 0)
	    {
		if ((c = in.read()) == in.EOF)
		    throw new IOException("EOF in char-code");
		if ('0' <= c && c <= '7')
		    c -= '0';
		else
		    throw new IOException("non-octal digit in char-code");
		result = (result * 8) + c;
	    }
	    return result;
	}

	/**
	 * Reads an N-digit hexadecimal integer literal from the input.
	 *
	 * @param len  the number of digits expected
	 @ @return the integer value
	 @ @throws IOException for input stream errors, or invalid digits
	 */
	public int getHex (int len)
	    throws IOException
	{
	    int result = 0, c;

	    while (len-- > 0)
	    {
		if ((c = in.read()) == in.EOF)
		    throw new IOException("EOF in char-code");
		if ('0' <= c && c <= '9')
		    c -= '0';
		else if ('a' <= c && c <= 'f')
		    c -= 'a' - 10;
		else if ('A' <= c && c <= 'F')
		    c -= 'A' - 10;
		else
		    throw new IOException("non-hexadecimal digit in char-code");
		result = (result * 16) + c;
	    }
	    return result;
	}

	/**
	 * Gets the current input encoding.
	 * This will correspond to the last "set-file-encoding-method"
	 * directive processed in the file.
	 *
	 * @return the Monk name of the encoding
	 */
	public String getEncoding ()
	    { return code; }

	/**
	 * Sets encoding, using the Monk symbol names.
	 *
	 * @param name  the Monk name of the encoding
	 * @throws IOException for unsupported encoding names
	 */
	public void setEncoding (String name)
	    throws IOException
	{
	    if (debug) System.out.print("[ - set encoding=" + name + " ]");
	    if (name.startsWith(":"))
		name = name.substring(1);

	    if (name.equals("ASCII"))
		in.setEncoding(MulticodeStreamReader.USASCII);
	    else if (name.equals("UTF8"))
		in.setEncoding(MulticodeStreamReader.UTF_8);
	    else if (name.equals("SJIS"))
		in.setEncoding(MulticodeStreamReader.SHIFT_JIS);
	    else if (name.equals("UHC"))
		in.setEncoding(MulticodeStreamReader.MS949);
	    else
		throw new IOException("unknown encoding \"" + name + "\"");
	    code = name;
	}

	/**
	 * Gets string literal, with given encoding.
	 * The string is scanned both as a character sequence and as a byte
	 * sequence, so the original stream input can be reproduced without
	 * requiring an encoder.
	 *
	 * @param mcCode  the encoding, as a MulticodeStreamReader constant
	 * @param code  the encoding, as a Monk encoding name string
	 * @return the scanned Monk string object
	 * @throws IOException for input stream problems, and malformed strings
	 */
	private MonkString stringFetch (byte mcCode, String code)
	    throws IOException
	{
	    int c = in.read();
	    if (c != '"')
		die("Monk.scan: expecting start of string");
	    MonkLoc from = here(); // ...may span many lines
	    StringBuffer sb = new StringBuffer();
	    MulticodeStreamReader.MulticodePos start = in.tell(), end;
	    in.setEncoding(mcCode);
	    while ((c = in.read()) != '"')
	    {
		if (c == in.EOF)
		    die("Monk.scan: EOF in string");
		if (c == '\n')
		    line ++;
		if (c == '\\')
		{
		    // Possible escape.
		    c = in.read();
		    switch (c)
		    {
		    case MulticodeStreamReader.EOF:
			die("Monk.scan: EOF in escape");

		    case '"':
		    case '\\':
			// Escape directly.
			break;

		    case 'a': c = '\007'; break;
		    case 'b': c = '\010'; break;
		    case 'f': c = '\014'; break;
		    case 'n': c = '\012'; break;
		    case 'r': c = '\015'; break;
		    case 't': c = '\011'; break;
		    case 'v': c = '\013'; break;

		    case 'o':
			// 3-digit octal char-code.
			c = getOct(3);
			break;
		    case 'x':
			// 2-digit hexadecimal char-code.
			c = getHex(2);
			break;
		    case 'u':
			// 4-digit hexadecimal char-code.
			c = getHex(4);
			break;

		    default:
			/* Not an escape.  Note that Monk allows this:
			 * a backslash followed by any char X where
			 * "\X" is not a legal escape is the same as
			 * writing "\\X", i.e. the backslash is itself.
			 */
			sb.append('\\');
		    }
		}
		sb.append((char) c);
	    }
	    int quoteBytes = in.readBytes;
	    end = in.tell();
	    int byteCount = (int) end.offset(start) - quoteBytes;
	    if (debug)
		System.out.println("[ - string=" + sb.length()
		    + ", " + byteCount + " bytes ]");
	    in.seek(start);
	    in.setEncoding(MulticodeStreamReader.RAWBYTES);
	    byte[] bytes = new byte[byteCount];
	    for (int i = 0; i < byteCount; i ++)
		bytes[i] = (byte) in.read();
	    in.seek(end);
	    return new MonkString(from, sb, bytes, code);
	}

	/**
	 * Gets next token from input.
	 *
	 * @throws IOException  for errors in input stream, and syntax errors
	 */
	public MonkData next ()
	    throws IOException
	{
	    int c;

	    if (debug) System.out.print("[ next " + seqno + " ]");
	    if (in == null)
		throw new IOException("MonkLexer: already closed");
	    while ((c = in.readGet()) != in.EOF)
	    {
		if (debug) System.out.print("<" + charStr(c) + ">");
		switch (c)
		{
		case '\n':
		    // New line.
		    line ++;
		    if (debug)
			System.out.println("[ - line " + line + " ]");
		    break;

		case ' ':
		case '\b':
		case '\r':
		case '\t':
		case '\013':
		case '\014':
		    // Whitespace.
		    break;

		case ';':
		    // Comment to end-of-line.
		    while ((c = in.read()) != in.EOF && c != '\n') {}
		    line ++;
		    break;

		case '\'':
		    // Alternate notation for "(quote ...)".
		    MonkLoc from0 = here();
		    MonkData quote = next();
		    switch (quote.type)
		    {
		    case MonkData.SC_EOF:
		    case MonkData.SC_DOT:
		    case MonkData.SC_CLOSE:
			die("cannot quote " + quote.typeName());
		    }
		    return new MonkPair(from0,
			new MonkSymbol(from0, "quote"),
			new MonkPair(from0, quote, new MonkNil(from0)));

		case '(':
		    // Sublist.
		    Vector elems = new Vector();
		    boolean gotDot = false;
		    MonkData elem;
		    MonkLoc from1 = here();
		    if (debug) System.out.println("[ - sublist ]");
		    while (true)
		    {
			elem = next();
			switch (elem.type)
			{
			case MonkData.SC_CLOSE:
			    // List complete.
			    int i = elems.size();
			    MonkData result;
			    if (gotDot)
			    {
				if (i < 2)
				    throw new IOException
					("misplaced dot in list");
				result = (MonkData) elems.get(-- i);
			    }
			    else
			    {
				// Regular list, ends in nil.
				result = new MonkNil(from1);
			    }
			    while (i-- > 0)
			    {
				// Cons up the elements.
				elem = (MonkData) elems.get(i);
				result = new MonkPair(elem.loc, elem, result);
			    }
			    if (debug)
				System.out.println("[ - sublist "
				    + elems.size()
				    + (gotDot ? " (dotted)" : "") + " ]");
			    return result;

			case MonkData.SC_EOF:
			    throw new IOException("MonkScan: EOF in list");

			case MonkData.SC_DOT:
			    // Should only occur 2nd from last.
			    gotDot = true;
			    break;

			default:
			    // Add to element list.
			    elems.add(elem);
			}
		    }
		    //=break;

		case ')':
		    return new MonkSpecial(here(), MonkData.SC_CLOSE);
		case '.':
		    // NYI: symbols starting with "."
		    return new MonkSpecial(here(), MonkData.SC_DOT);

		case '"':
		    // String literal, without prefix (i.e. current encoding).
		    if (debug)
			System.out.println("[ - string ]");
		    in.unget();
		    return stringFetch(in.getEncoding(), code);

		case '#':
		    // All kinds of special notations.
		    switch (c = in.read())
		    {
		    case 'f':
			// "#f" : boolean literal.
			return new MonkBoolean(here(), false);
		    case 't':
			// "#t" : boolean literal.
			return new MonkBoolean(here(), true);

		    case 'A':
			/* Must be string prefix: "#ASCII".
			 * NOTE: We should find a less repetitive way of
			 * dealing with encoding prefixes.  Unfortunately,
			 * we don't know where an encoding prefix ends in
			 * theory, because the encoding of what follows
			 * depends on the prefix.
			 */
			if (in.read() != 'S' ||
			    in.read() != 'C' ||
			    in.read() != 'I' ||
			    in.read() != 'I')
			{
			    // Impossible.
			    throw new IOException
				("MonkScan: illegal string encoding prefix");
			}
			return stringFetch(MulticodeStreamReader.USASCII,
			    "ASCII");

		    case 'S':
			// Must be string prefix: "#SJIS".
			if (in.read() != 'J' ||
			    in.read() != 'I' ||
			    in.read() != 'S')
			{
			    // Impossible.
			    throw new IOException
				("MonkScan: illegal string encoding prefix");
			}
			return stringFetch(MulticodeStreamReader.SHIFT_JIS,
			    "SJIS");

		    case 'U':
			// Must be string prefix: "#UTF8".
			switch (in.read())
			{
			case 'H':
			    if (in.read() == 'C')
				return stringFetch(MulticodeStreamReader.MS949,
				    "UHC");
			    break;
			case 'T':
			    if (in.read() == 'F' && in.read() == '8')
				return stringFetch(MulticodeStreamReader.UTF_8,
				    "UTF8");
			    break;
			}
			// Impossible.
			throw new IOException
			    ("MonkScan: illegal string encoding prefix");

		    case '\\':
			// Character literal.
			if ((c = in.read()) == in.EOF)
			    throw new IOException
				("MonkScan: EOF in char-literal");
			//NYI: #\xFF, #\u1234, #\newline, etc.
			return new MonkCharacter(here(), c);

		    case '(':
			// Vector literal.
			Vector vmems = new Vector();
			MonkData vmem;
			MonkLoc from3 = here();
			if (debug) System.out.println("[ - vector ]");
			while (true)
			{
			    vmem = next();
			    switch (vmem.type)
			    {
			    case MonkData.SC_CLOSE:
				// List complete.
				int len = vmems.size();
				MonkVector mv = new MonkVector(from3, len);
				for (int i = 0; i < len; i ++)
				    mv.value[i] = (MonkData) vmems.get(i);
				return mv;

			    case MonkData.SC_EOF:
				throw new IOException
				    ("MonkScan: EOF in vector");

			    case MonkData.SC_DOT:
				// Should only occur in sublists.
				throw new IOException
				    ("MonkScan: dot in vector");

			    default:
				// Add to element list.
				vmems.add(vmem);
			    }
			}
			//=break;

		    default:
			throw new IOException("Monk.scan: illegal char "
			    + charStr(c) + " after #");
		    }
		    //=break;

		case '-':
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		    // Numeric literal.
		    if (debug) System.out.println("[ - number ]");
		    StringBuffer nb = new StringBuffer();
		    nb.append((char) c);
		    while (true)
		    {
			if ((c = in.readGet()) == MulticodeStreamReader.EOF)
			    break;
			if (c < '0' || '9' < c)
			    { in.unget(); break; }
			nb.append((char) c);
		    }
		    String ns = nb.toString();
		    if (ns.equals("-"))
		    {
			// Just a single "-", operator instead of number.
			return new MonkSymbol(here(), "-");
		    }
		    if (debug) System.out.println("[ - number <" + ns + "> ]");
		    return new MonkInteger(here(), true, Long.parseLong(ns));

		case '~':
		    // Monk path expression.
		    StringBuffer pb = new StringBuffer();
		    while (true)
		    {
			if ((c = in.readGet()) == MulticodeStreamReader.EOF)
			    break;
			if ((monkId[c] & PATH) == 0)
			    { in.unget(); break; }
			pb.append((char) c);
		    }
		    return new MonkSymbol(here(), pb.toString());

		default:
		    if ((monkId[c] & IDC1) != 0)
		    {
			// Looks like a symbol.
			StringBuffer id = new StringBuffer((char) c);
			id.append((char) c);
			while (true)
			{
			    if ((c = in.readGet()) == MulticodeStreamReader.EOF)
				break;
			    if ((monkId[c] & IDCH) == 0)
				{ in.unget(); break; }
			    if (debug) System.out.print("{" + charStr(c) + "}");
			    id.append((char) c);
			}
			return new MonkSymbol(here(), id.toString());
		    }
		    die("illegal initial char " + charStr(c)
			    + " (current encoding: " + code + ")");
		}
	    }
	    in.close();
	    return new MonkSpecial(here(), MonkData.SC_EOF);
	}

	/**
	 * Closes this input stream, releasing all resources.
	 */
	public void close ()
	    throws IOException
	{
	    in.close();
	    in = null;
	    file = null;
	    line = 0;
	}
    }

    /**
     * Represents a Monk SSC file, minus the comment.
     */
    public static class MonkSsc
    {
	public static boolean debug = false;
	public Vector item;
	public String code;

	/**
	 * Reads given file.  Scans for encoding.
	 *
	 * @param name  the file to parse
	 * @throws IOException for problems reading the input
	 */
	public MonkSsc (String name)
	    throws IOException
	{
	    item = new Vector();
	    MonkLexer in = new MonkLexer(name);
	    in.debug = debug;
	    while (true)
	    {
		MonkData m = in.next();
		if (m.type == MonkData.SC_EOF)
		    break;
		item.add((Object) m);
		// Check for change of encoding.
		if (m.type == MonkData.PAIR)
		{
		    MonkPair mp = (MonkPair) m;
		    if (mp.car.type == MonkData.SYMBOL &&
			((MonkSymbol) mp.car).value.equals
			    ("set-file-encoding-method") &&
			mp.cdr.type == MonkData.PAIR &&
			((MonkPair) mp.cdr).car.type == MonkData.SYMBOL)
		    {
			code = ((MonkSymbol)((MonkPair) mp.cdr).car).value;
			in.setEncoding(code);
		    }
		}
	    }
	    code = in.getEncoding();
	    in.close();
	}
    }

    /**
     * Only for testing: given a list of files, reads each file as Monk code,
     * then prints from internal representation.
     *
     * @param args  the list of filenames
     */
    public static void main (String[] args)
    {
	int c = 0;
	boolean debug = false;
	Option opt = new Option(args, "d");
	while ((c = opt.getOpt()) != Option.EOF)
	{
	    switch (c)
	    {
	    case 'd':
		// Option -d: switch on debugging.
		debug = true;
		break;
	    default:
		// Huh?
		throw new RuntimeException("Unknown option: -" + c);
	    }
	}
	int oind = opt.getOptInd();
	try
	{
	    MonkData m;

	    MonkSsc.debug = debug;
	    for (int i = oind; i < args.length; i ++)
	    {
		if (debug)
		{
		    System.out.println("[ parse file <" + args[i] + "> ]");
		    System.out.flush();
		}
		MonkSsc ssc = new MonkSsc(args[i]);
		System.out.println("[ code=" + ssc.code + " ]");
		for (int j = 0; j < ssc.item.size(); j ++)
		{
		    m = (MonkData) ssc.item.get(j);
		    System.out.println("==== file <" + m.loc.getFile()
			+ ">, line #" + m.loc.getLine());
		    (new MonkPrinter(System.out)).print(m);
		}
		if (debug)
		    System.out.println("[ leave file <" + args[i] + "> ]");
	    }
	}
	catch (Exception e)
	{
	    e.printStackTrace(System.err);
	    System.exit(1);
	}
    }
}
