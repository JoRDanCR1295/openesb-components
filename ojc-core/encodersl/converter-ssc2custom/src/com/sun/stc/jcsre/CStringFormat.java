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
 * @(#)CStringFormat.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.text.FieldPosition;
import java.text.ParsePosition;
import java.text.ParseException;

import com.sun.stc.eways.util.StringUtils;

/**
 * a class to format strings similar to prinf
 *
 * @author Scott Steadman ssteadman@seebeyond.com
 */
public class CStringFormat extends StringFormat {

    private static final char PCT = '%';

    private static final String VALID_FLAGS = "-^+";
    private static final String VALID_ARGSIZES = "bBhl";
    private static final String VALID_ARGTYPES = "s";

    private String pattern = null;

    private char flag = '\u0000';
    private int width = -1;
    private int precision = -1;
    private char argSize = '\u0000';
    private char argType = '\u0000';

    
    /**
     * hidden default constructor
     */
    private CStringFormat(){super();}

    /**
     * construct an instance that formats strings based on 
     * specified pattern
     *
     * @param _pattern the pattern
     */
    public CStringFormat(String _pattern) {
        parsePattern(_pattern);
    }

    /**
     * parse the pattern and set internal variables
     *
     * @param _pattern pattern to parse
     *
     * @throws IllegalArgumentException _pattern is invalid
     */
    private void parsePattern (String _pattern) throws IllegalArgumentException {

        if(null == _pattern) throw new IllegalArgumentException("pattern is null.");

        this.pattern = _pattern;

        char[] chars = _pattern.toCharArray();

        if(chars[0] != PCT) throw new IllegalArgumentException("pattern must conform to C conventions and begin with a " + PCT);

        int pos = 1;

        // handle %%
        if(PCT == chars[pos]) {
            this.flag = PCT;
            return;
        }

        // parse optional flag
        if(VALID_FLAGS.indexOf(chars[pos]) > -1) {
            this.flag = chars[pos++];
        }

        // parse optional width
        if(Character.isDigit(chars[pos])) {
            this.width = 0;
            while(Character.isDigit(chars[pos])) {
                this.width *= 10;
                this.width += chars[pos] - '0';
                pos++;
            }
        }

        // parse optional precision
        if('.' == chars[pos]) {
            pos++;
            this.precision = 0;
            while(Character.isDigit(chars[pos])) {
                this.precision *= 10;
                this.precision += chars[pos] - '0';
                pos++;
            }
        }

        // parse optional argSize
        if(VALID_ARGSIZES.indexOf(chars[pos]) > -1) {
            this.argSize = chars[pos++];
        }

        // verify mandatory argType
        if(-1 == VALID_ARGTYPES.indexOf(chars[pos])) {
            throw new IllegalArgumentException("invalid argType: " + chars[pos]);
        }

        // parse argType
        this.argType = chars[pos++];

    }

    /**
     * @see java.text.Format#clone()
     *
     * @return a deep copy of the instance
     */
    public Object clone () {
        CStringFormat format = new CStringFormat();

        format.pattern = this.pattern;
        format.flag = this.flag;
        format.width = this.width;
        format.precision = this.precision;
        format.argSize = this.argSize;
        format.argType = this.argType;

        return format;
    }

    /**
     * format a string value
     * 
     * @param _val value
     * @param _buf buffer
     * @param _pos field position
     *
     * @returns _buf with formatted text appended
     */
    public StringBuffer format (Object _val, StringBuffer _buf, FieldPosition _pos) {

        // quick return
        if(null == _val) return _buf;

        String val = _val.toString();

        // truncate 
        if(this.precision != -1 && this.precision < val.length()) {
            val = val.substring(0, this.precision);
        }

        // pad
        if(this.width != -1) {
            if('-' == this.flag) {
                _buf.append(StringUtils.padRight(val, this.width));
            } else if('^' == this.flag) {
                _buf.append(StringUtils.padCenter(val, this.width));
            } else {
                _buf.append(StringUtils.padLeft(val, this.width));
            }

        } else {
            _buf.append(val);
        }

        return _buf;
    }


    /**
     * @see java.text.Format#parseObject(String,ParsePosition)
     */
    public Object parseObject (String _string, ParsePosition _pos) {
        return null;
    }


    /**
     * convert the format to a string
     *
     * @return string representing format
     */
    @Override
    public String toString () {
        StringBuffer buf = new StringBuffer("(CStringFormat");

        buf.append(" pattern='").append(this.pattern).append("'");
        buf.append(" flag=").append(this.flag);
        buf.append(" width=").append(this.width);
        buf.append(" precision=").append(this.precision);
        buf.append(" argSize=").append(this.argSize);
        buf.append(" argType=").append(this.argType);

        return buf.append(")").toString();
    }

    public static void main (String[] _args) {

        Object[] args = new Object[] {
            new Object[]{"%10s" ,"test"}
            ,new Object[]{"%-10s" ,"test"}
            ,new Object[]{"%.3s" ,"testing"}
            ,new Object[]{"%-.3s" ,"testing"}
        };

        for(int ii=0; ii<args.length ;ii++) {
            Object[] parts = (Object[])args[ii];
            String format = (String)parts[0];
            String str = (String)parts[1];
            System.out.println(format + "("+str+"): " + new CStringFormat(format).format(str));
        }
    }

}
    
