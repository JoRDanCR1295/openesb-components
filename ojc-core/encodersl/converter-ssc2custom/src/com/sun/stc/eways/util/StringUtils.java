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
 * @(#)StringUtils.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.eways.util;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

public class StringUtils {

    /**
     * default escape character
     */
    public static final char DEFAULT_ESCAPE = '\\';

    /**
     * hidden constructor to prevent instantiation
     */
    private StringUtils (){}

    /**
     * condense duplicate strings to a single instance within a string
     *
     * @param _string string to consense
     * @param _toRemove string to remove
     *
     * @return condensed string
     */
    public static String condenseDups (String _string, String _toRemove) {
        String dup = _toRemove+_toRemove;

        // cache length
        int len=_toRemove.length();

        for(int pos=_string.indexOf(dup)
            ;pos>-1
            ;pos=_string.indexOf(dup, pos)
        ) {
            _string = _string.substring(0, pos+len)
                    + _string.substring(pos+(len<<1));
            pos+=len;
        }

        return _string;
    }

    /**
     * expand substrings within a string
     *
     * @param _string string to expand
     * @param _toDup string to duplicate
     *
     * @return expanded string
     */
    public static String expandDups (String _string, String _toDup) {

        // cache length
        int len=_toDup.length();

        for(int pos=_string.indexOf(_toDup)
            ;pos>-1
            ;pos=_string.indexOf(_toDup, pos)
        ) {
            _string = _string.substring(0, pos+len)
                    + _toDup
                    + _string.substring(pos+len);
            pos+=len<<1;
        }

        return _string;
    }

    /**
	* Prepends the specified character to a string until the length is reached. If the string is already larger than the specified length, the string itself is returned.
	* <p>
	* @param _string The string to be padded.
	* @param _len The minimum length of the padded string.
	* @param _char The padding character.
	* @return <CODE>String</CODE> - The padded string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String padLeft (String _string, int _len, char _char) {

        // substitute empty string if null
        if(null == _string) _string = "";

        StringBuffer buf = new StringBuffer();
        for(int ii=_len - _string.length(); ii>0 ;ii--) {
            buf.append(_char);
        }

        buf.append(_string);

        return buf.toString();
    }

    /**
	* Prepends spaces to a string until the length is reached. If the string is already longer than the specified length, the string itself is returned.
	* <p>
	* @param _string The string to be padded.
	* @param _len The number of spaces to pad to the string.
	* @return <CODE>String</CODE> - The padded string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String padLeft (String _string, int _len) {
        return StringUtils.padLeft(_string, _len, ' ');
    }

    /**
	* Appends the specified character to a string until the length is reached. If the string is already larger than the specified length, the string itself is returned.
	* <p>
	* @param _string The string to be padded.
	* @param _len The minimum length of the padded string.
	* @param _char The padding character.
	* @return <CODE>String</CODE> - The padded string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String padRight (String _string, int _len, char _char) {

        // substitute empty string if null
        if(null == _string) _string = "";

        StringBuffer buf = new StringBuffer();
        buf.append(_string);
        for(int ii=_len - _string.length(); ii>0 ;ii--) {
            buf.append(_char);
        }

        return buf.toString();
    }

    /**
	* Appends spaces to a string until the length is reached. If the string is already larger than the specified length, the string itself is returned.
	* <p>
	* @param _string The string to be padded.
	* @param _len The minimum length of the padded string.
	* @return <CODE>String</CODE> - The padded string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String padRight (String _string, int _len) {
        return StringUtils.padRight(_string, _len, ' ');
    }

    /**
	* Centers a string and pads it with the specified characters to make it the specified length. If the string is already larger than the specified length, the string itself is returned.
	* <p>
	* @param _string The string to be padded.
	* @param _len The minimum length of the padded string.
	* @param _char The padding character.
	* @return <CODE>String</CODE> - The padded string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String padCenter (String _string, int _len, char _char) {

        // substitute empty string if null
        if(null == _string) _string = "";

        int spaceLeft = _len - _string.length();
        if(spaceLeft < 0) return _string;

        StringBuffer buf = new StringBuffer();

        int spaces = spaceLeft>>1;
        for(int ii=spaces; ii>0 ;ii--) {
            buf.append(_char);
        }

        buf.append(_string);
        buf.append(buf.substring(0, spaces));

        // append extra space if necessary
        if(buf.length()<_len) buf.append(_char);

        return buf.toString();
    }

    /**
	* Centers a string and pads it with spaces to make it the specified length. If the string is already larger than the specified length, the string itself is returned.
	* <p>
	* @param _string The string to be padded.
	* @param _len The minimum length of the padded string.
	* @return <CODE>String</CODE> - The centered and trimmed string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String padCenter (String _string, int _len) {
        return StringUtils.padCenter(_string, _len, ' ');
    }

    /**
	* Trims the leading characters from a string.
	* <p>
	* @param _string The string to be trimmed.
	* @param _chars The characters to trim.
	* @return <CODE>String</CODE> - The trimmed string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String trimLeft (String _string, String _chars) {

        // no chararacters to trim
        if(empty(_chars)) return _string;

        // no string
        if(empty(_string)) return _string;

        // no trimming necessary
        if(_chars.indexOf(_string.charAt(0)) == -1) return _string;

        // find start position
        int start = 1;
        while(_chars.indexOf(_string.charAt(start)) > -1) start++;

        return _string.substring(start);
    }

    /**
	* Trims the trailing characters from a string.
	* <p>
	* @param _string The string to be trimmed.
	* @param _chars The characters to trim.
	* @return <CODE>String</CODE> - The trimmed string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
        
        //
        // ESR 80870, HCG, 02/04/05:
        //    I am merely checking in a solution that was
        //    provided by Steve Collins. So far it has passed testing
        //    in the field as specified by Jinhee Eom. I also did
        //    a unit test in my environment and it passed my tests as
        //    specified in this ESR. However, this new solution to 
        //    trimRight needs to be tested and approved by QA Testing.
        //
        public static String trimRight (String _string, String _chars) {

	    if(_string==null || _chars==null)
		return _string;

	    if (_string.length()==0 || _chars.length()==0)
		return _string;

	    // find end position
	    char c;
	    int end;
	    for(end = _string.length()-1; end >=0; end--)
		{
		    // get the last char of source
		    c = _string.charAt(end);
		    // if it's in the mask, continue
		    if(_chars.indexOf(c) > -1)
			continue;
		    break;
		}

	    return _string.substring(0, end+1);
	    
	}
        
        /*
	public static String trimRight (String _string, String _chars) {

        // no chararacters to trim
        if(empty(_chars)) return _string;

        // no string
        if(empty(_string)) return _string;

        //com.sun.stc.common.collabService.EGate.collabTrace("TrimRight: _string.length(): "+_string.length());

        if (_string.length() == 0)
          {
            //
            // no trimming necessary
            //
            return _string;
          }

        // no trimming necessary
        if(_chars.indexOf(_string.charAt(_string.length()-1)) == -1) return _string;

        // find end position
        int end = _string.length()-1;
        //
        // QAI 52597.  Need to make sure string index is not out of bound.
        //
        while(_string.charAt(end) < _chars.length() &&
              _chars.indexOf(_string.charAt(end)) > -1)
          {
            end--;
          }

        return _string.substring(0, end+1);
    }
     */

    /**
	* Trims the specified characters from both ends of a string.
	* <p>
	* @param _string The string to be trimmed.
	* @param _chars The characters to trim.
	* @return <CODE>String</CODE> - The trimmed string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String trimBoth (String _string, String _chars) {
        return trimLeft(trimRight(_string, _chars), _chars);
    }

    /**
	* Escapes all instances of a string in the specified input string with the <code><b>DEFAULT_ESCAPE</code></b> character.
	* <p>
	* @param _string The input string to escape.
	* @param _toEscape The characters to escape.
	* @return <CODE>String</CODE> - The escaped string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String escape (String _string, String _toEscape) {
        return escape(_string, _toEscape, DEFAULT_ESCAPE);
    }

    /**
	* Escapes all instances of a string in the specified input string with the specified escape character. If any instances of <code><b>_escapeChar</code></b> occur in the string, it will also be escaped.
	* <p>
	* @param _string The input string to escape.
	* @param _toEscape The characters to escape.
	* @param _escapeChar The character used to perform the escape.
	* @return <CODE>String</CODE> - The escaped string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String escape (String _string, String _toEscape, char _escapeChar) {

        for(int pos=_string.indexOf(_escapeChar)
            ;pos>-1
            ;pos=_string.indexOf(_escapeChar, pos)
        ) {
            _string = _string.substring(0, pos)
                    + _escapeChar
                    + _string.substring(pos);
            pos+=2;
        }

        int escLen = _toEscape.length();
        for(int pos=_string.indexOf(_toEscape)
            ;pos>-1
            ;pos=_string.indexOf(_toEscape, pos)
        ) {
            _string = _string.substring(0, pos)
                    + _escapeChar
                    + _string.substring(pos);
            pos+=1+escLen;
        }

        return _string;
    }

    /**
	* Removes any <code><b>DEFAULT_ESCAPE</code></b> characters from the specified string.
	* <p>
	* @param _string The string containing the escape characters.
	* @return <CODE>String</CODE> - The unescaped string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String unescape (String _string) {
        return unescape(_string, DEFAULT_ESCAPE);
    }

    /**
	* Removes any escape characters from the specified string.
	* <p>
	* @param _string The string containing the escape characters.
	* @param _escapeChar The characters used to perform the escape.
	* @return The unescaped string.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static String unescape (String _string, char _escapeChar) {

        for(int pos=_string.indexOf(_escapeChar)
            ;pos>-1
            ;pos=_string.indexOf(_escapeChar, pos)
        ) {
            _string = _string.substring(0, pos)
                    + _string.substring(pos+1);
            pos++;
        }

        return _string;
    }

    /**
     * return true if the string is null or empty
     *
     * @param _str the string in question
     *
     * @return true if _str is null or ""
     */
    public static boolean empty (String _str) {
        return (null==_str || _str.equals(""));
    }

    /**
	* Returns a string containing the error message and stack trace for the specified exception.
	* <p>
	* @param _ex The exception.
	* @return <CODE>String</CODE> - The error message and stack trace.
	* <DT><B>Throws:</B><DD>None.
	* @see java.lang.Exception#printStackTrace(PrintStream)
	* @include
	*/
	public static String toString (Exception _ex) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        _ex.printStackTrace(new PrintStream(baos));
        return baos.toString();
    }

}
