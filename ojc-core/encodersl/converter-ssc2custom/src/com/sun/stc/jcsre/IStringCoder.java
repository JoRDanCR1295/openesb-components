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
 * @(#)IStringCoder.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

//-import com.sun.stc.jcsre.StringCoderException;
import com.sun.stc.jcsre.PropertyException;

/**
 * Interface for objects that can encode String values as byte-arrays,
 * and decode byte-arrays to String values.
 */
public interface IStringCoder
{
    /**
     * Convert a string into a byte array.
     *
     * @param _s  a string, or null
     *
     * @return byte array containing the encoded string
     */
    public byte[] encode (String _s);

    /**
     * Convert a single character into a byte array.
     *
     * @param c  a Unicode character
     *
     * @return byte array containing the encoded string
     */
    public byte[] encodeChar (char c);

    /**
     * Convert a byte array into a string.
     *
     * @param _b  null or a byte array containing the encoded string
     *
     * @return the decoded string
     */
    public String decode (byte[] _b);

    /**
     * Convert a byte array into a character.
     * Will thow an IllegalArgumentException if full decoding would result in
     * more or fewer than one character.
     *
     * @param b  a byte array containing the encoded character
     *
     * @return the decoded character
     */
    public char decodeChar (byte[] b);

    /**
     * For future extension.
     *
     * @param _name  name of property
     * @param _value  new value of property
     *
     * @throws PropertyException if the property is not supported by the
     * encoder.
     */
    public void setProperty (String _name, Object _value)
        throws PropertyException;
    
    /**
     * For future extension.
     *
     * @param _name  name of property
     * @return value of property
     *
     * @throws PropertyException if the property is not supported by the
     * encoder.
     */
    public Object getProperty (String _name) throws PropertyException;

}
