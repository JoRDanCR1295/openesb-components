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
 * @(#)StringFile8.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre.ssc;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;

import java.util.StringTokenizer;
import java.util.Arrays;

import com.sun.stc.jcsre.IStringCoder;
import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.PropertyException;

/**
 * An implementation of the string coder interface, that supports
 * stateless 8-bit character sets, and takes its encoding from a table
 * in a text file.
 *
 * The encoding files are all sought relative to the directory provided
 * by the JCS property "StringFile8.dir".  Encoding files consist of
 * line of the following format:
 *
 *   [ <byte-value> { <Unicode-value> | "=" } ] [ "#" comment ]
 *
 * in other words a byte-value (hexadecimal) is equated with a set of
 * Unicode values (hexadecimal, or "=" meaning same a byte-value).
 * When mapping Unicode to bytes, the last of these is taken.  If a mapping
 * is not defined for some value, we throw IllegalArgumentException during
 * the conversion if it occurs.
 */
public class StringFile8 implements IStringCoder
{
    private String encoding = null;
    private String mapDir = JCSProperties.getProperty("StringFile8.dir");
    private char[] u2c;
    private char[] c2u;

    /**
     * Creates from encoding name.
     *
     * @param enc  name of encoding
     * @throws UnsupportedEncodingException encoding not supported
     */
    public StringFile8 (String enc) throws UnsupportedEncodingException
    {
	if (enc == null)
	    throw new UnsupportedEncodingException("null encoding name");
	if (mapDir == null || mapDir.equals(""))
	    throw new UnsupportedEncodingException("StringFile8.dir not set");
	File file = new File(mapDir, enc);
	LineNumberReader in = null;
	try
	{
	    // Try to open the file.
	    in =
		new LineNumberReader(
		    new InputStreamReader(
			new FileInputStream(file),
			"UTF-8"));
	}
	catch (UnsupportedEncodingException u)
	{
	    throw new UnsupportedEncodingException("platform has no UTF-8?");
	}
	catch (FileNotFoundException f)
	{
	    throw new UnsupportedEncodingException("no code file [" +
		file.getPath() + ']');
	}

	// Initialize the mapping.
	u2c = new char[0x10000];
	c2u = new char[0x100];

	Arrays.fill(u2c, '\uFFFF');
	Arrays.fill(c2u, '\uFFFF');

	String line, tok;
	for (;;)
	{
	    try { line = in.readLine(); }
	    catch (IOException io)
	    {
		throw new UnsupportedEncodingException(
		    "error reading code file [" + file.getPath() + "]: " +
			io.getMessage());
	    }
	    if (line == null) { break; }
	    int i;
	    if ((i = line.indexOf('#')) >= 0)
	    {
		// Strip comment.
		line = line.substring(0, i);
	    }
	    if (line.equals(""))
	    {
		// Skip empty lines.
		continue;
	    }
	    StringTokenizer st = new StringTokenizer(line);
	    if (st.hasMoreTokens())
	    {
		// First get the byte number.
		tok = st.nextToken();
		try
		{
		    i = Integer.parseInt(tok, 16);
		    if (i < 0 || 255 < i)
			throw new UnsupportedEncodingException(
			    "code file [" + file.getPath() + "], line "
			    + in.getLineNumber() +
			    ": 1st part not in range 0-FF [" + tok + "]");
		}
		catch (NumberFormatException n)
		{
		    throw new UnsupportedEncodingException(
			"code file [" + file.getPath() + "], line " +
			in.getLineNumber() +
			": 1st part not a number [" + tok + "]");
		}
		byte c = (byte) i;

		// Now get all mappings from Unicode.
		while (st.hasMoreTokens())
		{
		    tok = (st.nextToken());
		    try
		    {
			i = (tok.equals("=")
			    ? (c & 0xFF)
			    : Integer.parseInt(tok, 16));
			if (i < 0 || 0xFFFF < i)
			    throw new UnsupportedEncodingException(
				"code file [" + file.getPath() + "], line " +
				in.getLineNumber() +
				": 2nd part not in range 0-FFFF [" + tok + "]");
			char u = (char) i;
			c2u[c & 0xFF] = u;
			u2c[u & 0xFFFF] = (char) (c & 0x00FF);
		    }
		    catch (NumberFormatException n)
		    {
			throw new UnsupportedEncodingException(
			    "code file [" + file.getPath() + "], line " +
			    in.getLineNumber() +
			    ": 2nd part not a number [" + tok + "]");
		    }
		}
	    }
	}
	try { in.close(); }
	catch (IOException io)
	{
	    throw new UnsupportedEncodingException(
		"error closing code file [" + file.getPath() + "]: " +
		    io.getMessage());
	}
	this.encoding = enc;
    }

    /**
     * Converts a string into a byte array.
     *
     * @param s  a string, or null
     * @return byte array containing the encoded string
     * @throws IllegalArgumentException if s can't be converted
     */
    public byte[] encode (String s) throws IllegalArgumentException
    {
	if (null == s) { return null; }
	int len = s.length();
	if (len == 0) { return new byte[0]; }
	byte[] buf = new byte[len];
	for (int i = 0; i < len; i ++)
	{
	    int u = ((int) s.charAt(i) & 0xFFFF);
	    char c = u2c[u];
	    if (c == 0xFFFF)
		throw new IllegalArgumentException("can't map " + u +
		    ", at offset " + i);
	    buf[i] = (byte) c;
	}
	return buf;
    }

    /**
     * Converts a single character into a byte array.
     *
     * @param c  a Unicode character
     * @return byte array containing the encoded string
     * @throws IllegalArgumentException if c can't be converted
     */
    public byte[] encodeChar (char c) throws IllegalArgumentException
    {
	return encode(new Character(c).toString());
    }

    /**
     * Converts a byte array into a string.
     *
     * @param b  null or a byte array containing the encoded string
     * @return the decoded string
     * @throws IllegalArgumentException if b can't be converted
     */
    public String decode (byte[] b) throws IllegalArgumentException
    {
	if (b == null) { return null; }
	if (b.length == 0) { return ""; }
	char[] buf = new char[b.length];
	for (int i = 0; i < b.length; i ++)
	{
	    int c = (b[i] & 0xFF);
	    char u = c2u[c];
	    if (u == 0xFFFF)
		throw new IllegalArgumentException("can't map " + c +
		    ", at offset " + i);
	    buf[i] = u;
	}
	return new String(buf);
    }

    /**
     * Converts a byte array into a single character.
     *
     * @param b  null or a byte array containing the encoded string
     * @return the decoded character
     * @throws IllegalArgumentException if b can't be converted, or won't fit
     */
    public char decodeChar (byte[] b) throws IllegalArgumentException
    {
	String s = decode(b);
	if (s.length() != 1)
	    throw new IllegalArgumentException("decodeChar: got "
		+ s.length() + " chars, not 1");
	return s.charAt(0);
    }

    /**
     * For future extension.
     *
     * @param name name of property
     * @param value new value of property
     *
     * @throws PropertyException if the property is not supported
     */
    public void setProperty (String name, Object value)
	throws PropertyException
    {
	throw new PropertyException(getClass() +
	    " has no [" + name + "] property");
    }

    /**
     * For future extension.
     *
     * @param name  name of property
     * @return value of property
     *
     * @throws PropertyException if the property is not supported by the
     * encoder.
     */
    public Object getProperty (String name) throws PropertyException
    {
	throw new IllegalArgumentException(getClass() +
	" has no [" + name + "] property");
    }

    /**
     * File testing method.
     * Usage: main <file> <string>
     * Will try to convert given string back and forth using given encoding,
     * and report an error if this fails to produce the same result.
     *
     * @param args  command line arguments
     */
    public static void main (String[] args)
    {
	try
	{
	    StringFile8 coder = new StringFile8(args[0]);
	    String u1 = args[1];
	    byte[] ba = coder.encode(u1);
	    String u2 = coder.decode(ba);
	    if (! u1.equals(u2))
		throw new RuntimeException("encode differs from decode");
	}
	catch (Exception ex)
	{
	    ex.printStackTrace();
	}
    }
}
