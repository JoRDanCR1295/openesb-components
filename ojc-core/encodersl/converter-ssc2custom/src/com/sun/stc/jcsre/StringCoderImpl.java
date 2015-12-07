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
 * @(#)StringCoderImpl.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharacterCodingException;
import java.nio.CharBuffer;
import java.nio.ByteBuffer;

import java.io.UnsupportedEncodingException;

import com.sun.stc.jcsre.PropertyException;

/**
 * An implementation of the string coder interface, that serves as a
 * straight-forward map to the built-in encoding support of the JDK,
 * visible through java.lang.String.
 */
public class StringCoderImpl implements IStringCoder
{
    private static final boolean unitTest = false;

    private String encoding = null;
    private CharsetDecoder decoder = null;
    private CharsetEncoder encoder = null;

    /**
     * Create from encoding name.
     *
     * @param _enc  name of encoding
     *
     * @throws UnsupportedEncodingException encoding not supported
     */
    public StringCoderImpl (String _enc) throws UnsupportedEncodingException {
        if(_enc == null)
            throw new UnsupportedEncodingException("null encoding name");
        this.encoding = _enc;
        Charset ch = Charset.forName(_enc);
        this.decoder = ch.newDecoder();
        this.encoder = ch.newEncoder();
    }

    /**
     * Convert a string into a byte array.
     *
     * @param _s  a string, or null
     *
     * @return byte array containing the encoded string
     *
     * @throws IllegalArgumentException if _s can't be converted
     */
    public byte[] encode (String _s) throws IllegalArgumentException {
        if(null == _s) return null;
        try {
            return encoder.encode(CharBuffer.wrap(_s.toCharArray())).array();
        } catch(CharacterCodingException ex) {
            ex.printStackTrace();
            throw new IllegalArgumentException("Unable to convert string.");
        }

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
     * Convert a byte array into a string.
     *
     * @param _b  null or a byte array containing the encoded string
     *
     * @return the decoded string
     *
     * @throws IllegalArgumentException if _b can't be converted
     */
    public String decode (byte[] _b) throws IllegalArgumentException {
        if(null == _b) return null;
        try {
            return decoder.decode(ByteBuffer.wrap(_b)).toString();
        } catch(CharacterCodingException ex) {
            ex.printStackTrace();
            throw new IllegalArgumentException("Unable to convert byte[].");
        }
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
     * @param _name name of property
     * @param _value new value of property
     *
     * @throws PropertyException if the property is not supported by the
     * encoder.
     */
    public void setProperty (String _name, Object _value)
        throws PropertyException
    {
	throw new PropertyException(getClass() + " has no [" + _name + "] property");
    }

    /**
     * For future extension.
     *
     * @param _name  name of property
     * @return value of property
     *
     * @throws PropertyException if the property is not supported by the
     * encoder.
     */
    public Object getProperty (String _name) throws PropertyException {
	throw new IllegalArgumentException(getClass() + " has no [" + _name + "] property");
    }


    /**
     * Unit testing method
     */
    public static void main (String[] _args) {
        if(!unitTest) return;

        try {
            // test aliases
            IStringCoder coder = new StringCoderImpl("UTF8");
            coder = new StringCoderImpl("UTF-8");

            String theString = "This is a test: \u1000 \ufffe";
            byte[] bytes = coder.encode(theString);
            String newString = coder.decode(bytes);
            if(theString.equals(newString)) {
                System.out.println("Test passed.");
            } else {
                System.out.println("Test failed.");
            }
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }
}
