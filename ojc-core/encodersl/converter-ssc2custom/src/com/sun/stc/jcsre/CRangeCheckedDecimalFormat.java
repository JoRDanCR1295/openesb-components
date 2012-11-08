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
 * @(#)CRangeCheckedDecimalFormat.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.text.NumberFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;

import com.sun.stc.eways.util.StringUtils;

/**
 * class to parse and format numbers in a C format.
 *
 * @author Scott Steadman ssteadman@seebeyond.com
 */
public class CRangeCheckedDecimalFormat extends NumberFormat {

    private static final char PCT = '%';

    private static final String INT_ARGTYPES = "doxXzZ";
    private static final String REAL_ARGTYPES = "eEf";
    private static final String VALID_FLAGS = "-^+ 0#";
    private static final String VALID_ARGSIZES = "bBhl";
    private static final String VALID_ARGTYPES = INT_ARGTYPES + REAL_ARGTYPES;

    private String pattern = null;

    private String flags = "";
    private int width = -1;
    private int precision = -1;
    private char argSize = '\u0000';
    private char argType = '\u0000';
    private int base = 10;

    private String decimalFormatPattern = null;

    /**
     * create an empty instance
     */
    public CRangeCheckedDecimalFormat (){super();}

    /**
     * create an instance that parses/formats items in the
     * specified format
     *
     * @param _pattern the format
     *
     * @throws IllegalArgumentException _pattern is invalid
     */
    public CRangeCheckedDecimalFormat (String _pattern) throws IllegalArgumentException {
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
            this.flags = ""+PCT;
            return;
        }

        // parse optional flags
        int start = pos;
        while(VALID_FLAGS.indexOf(chars[pos]) > -1) pos++;
        if(pos>start) this.flags = _pattern.substring(start,pos);
//System.out.println("this.flag: " + this.flags);

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

        // hex
        if('x' == this.argType || 'X' == this.argType) this.base=16;

        // octal
        if('o' == this.argType) this.base=8;

        // arbitrary
        if('z' == this.argType || 'Z' == this.argType) {
            // skip [
            pos++;
            this.base = 0;
            while(Character.isDigit(chars[pos])) {
                this.base *= 10;
                this.base += chars[pos] - '0';
                pos++;
            }

            if(this.base>36) {
                throw new IllegalArgumentException("the base must be <= 36");
            }
            
        }

        // default to 6 digits after decimal if floating argType (%f or %e)
        if(-1 == this.precision
            && ('f' == this.argType || 'e' == this.argType)
        ) {
            this.precision = 6;
        }

        createDecimalFormatPattern();
    }

    /**
     * @see java.text.Format#clone()
     *
     * @return a deep copy of the instance
     */
    public Object clone () {
        CRangeCheckedDecimalFormat format = new CRangeCheckedDecimalFormat();

        format.pattern = this.pattern;
        format.decimalFormatPattern = this.decimalFormatPattern;
        format.flags = this.flags;
        format.width = this.width;
        format.precision = this.precision;
        format.argSize = this.argSize;
        format.argType = this.argType;
        format.base = this.base;

        return format;
    }

    /**
     * format an integer value
     * 
     * @param _val value
     * @param _buf buffer
     * @param _pos field position
     *
     * @returns _buf with formatted text appended
     */
    public StringBuffer format (long _val, StringBuffer _buf, FieldPosition _pos) {
        
        // reroute to format a double if float argtype
        if(REAL_ARGTYPES.indexOf(this.argType) > -1) {
            return format((double)_val, _buf, _pos);
        }

        // generate optional + sign
        String sign = "";
        if(_val >= 0) {
            if(hasFlag('+')) sign = "+";
        } else {
            _val = -_val;
            sign = "-";
        }

        // insert space if flags align
        if(sign.equals("") && hasFlag(' ') && !hasFlag('+') && _val>=0) {
            sign = " ";
        }

        // generate entire string
        String result = Long.toString(_val, this.base);
        if('Z' == this.argType || 'X' == this.argType) {
            result = result.toUpperCase();
        }


        // get padding character
        char pad = (hasFlag('0') && !hasFlag('-'))?'0':' ';

        // figure out minimum number of digits
        int size = this.width;
        if(-1 == size) {
            size = this.precision;
            if(!hasFlag('-')) pad = '0';
        }

        // pad string
        if(size > 0) {

            // left justify
            if(this.hasFlag('-')) {
                _buf.append(sign);
                _buf.append(result);

            // add sign if zero padded
            } else if('0' == pad) {
                _buf.append(sign);
            }

            if(result.length()+sign.length() < size) {
                for(int ii = size - result.length() - sign.length()
                    ;ii>0
                    ;ii--
                ) {
                    _buf.append(pad);
                }
            }

            // right justify
            if(!this.hasFlag('-')) {
                if('0' != pad) _buf.append(sign);
                _buf.append(result);
            }

        } else {
            _buf.append(sign);
            _buf.append(result);
        }

        return _buf;
    }

    /**
     * format a floating point value
     * 
     * @param _val value
     * @param _buf buffer
     * @param _pos field position
     *
     * @returns _buf with formatted text appended
     */
    public StringBuffer format (double _val, StringBuffer _buf, FieldPosition _pos) {

        // reroute to format a long if int argtype
        if(INT_ARGTYPES.indexOf(this.argType) > -1) {
            return format((long)_val, _buf, _pos);
        }

        StringBuffer result = new StringBuffer();

        FormatterFactory.getNumberFormat(this.decimalFormatPattern).format(_val, result, _pos);

        // change case of E if necessary
        if("eE".indexOf(this.argType)>-1) {
            int pos = result.length()-3;
            if("+-".indexOf(result.charAt(pos)) > -1) {
                result.replace(pos-1, pos, "e");
            } else if(result.charAt(pos) == 'E') {
                result.replace(pos, pos+1, "e+");
            }
        }

        // insert space if flagged
        int size = this.width;
        if(hasFlag(' ') && !hasFlag('+') && _val>=0) {
            _buf.append(' ');
            size--;
        }

        // left justify
        if(hasFlag('-')) {

            _buf.append(result);
            for(int ii=size - result.length() ;ii>0 ;ii--) {
                _buf.append(' ');
            }

        } else {

            // prepend padding characters
            char pad = hasFlag('0')?'0':' ';
            if(size > 0) {
                if(result.length() < size) {
                    for(int ii = size - result.length(); ii>0 ;ii--) {
                        _buf.append(pad);
                    }
                }
            }

            _buf.append(result);

        }

        // remove zero next to sign if result is too wide
        if(_buf.length() > width
            && '-' == _buf.charAt(0)
            && '0' == _buf.charAt(1)
        ) {
            _buf.deleteCharAt(1);
        }

        return _buf;
    }

    /**
     * create a java.text.DecimalFormat pattern based on this
     */
    private void createDecimalFormatPattern () {

        if(this.precision > 0) {
            this.decimalFormatPattern = StringUtils.padRight("0.", this.precision+2, '0');
        }

        char pad = (hasFlag('0') && !hasFlag('-'))?'0':'#';

        if(this.width > 0) {
            int size = this.width;

            // handle space or sign prepend
            if(hasFlag(' ') || hasFlag('+')) {
                size--;
            }

            // handle exponent (e+00)
            if("eE".indexOf(this.argType) > -1) {
                size -= 4;
            }

            this.decimalFormatPattern = StringUtils.padLeft(this.decimalFormatPattern, size, pad);
        } 

        if('e' == this.argType || 'E' == this.argType) {
            this.decimalFormatPattern += "E00";
        }

        if(hasFlag('+')) {
            this.decimalFormatPattern = "+"
                                    + this.decimalFormatPattern
                                    + ";-"
                                    + this.decimalFormatPattern
                                    ;
        }
//System.out.println(this.pattern +" -> "+this.decimalFormatPattern);

    }

    /**
     * return true if this.pattern contains the specified flag
     *
     * @param _flag flag character
     *
     * @return true if the flag is in the pattern
     */
    private boolean hasFlag (char _flag) {
        return this.flags.indexOf(_flag)>-1;
    }


    /**
     * @see java.text.NumberFormat#parse(String,ParsePosition)
     */
    public Number parse (String _string, ParsePosition _pos) {
        return FormatterFactory.getNumberFormat(this.decimalFormatPattern).parse(_string.trim(), _pos);
    }


    /**
     * convert the format to a string
     *
     * @return string representing format
     */
    @Override
    public String toString () {
        StringBuffer buf = new StringBuffer("(CRangeCheckedDecimalFormat");

        buf.append(" pattern='").append(this.pattern).append("'");
        buf.append(" flags=").append(this.flags);
        buf.append(" width=").append(this.width);
        buf.append(" precision=").append(this.precision);
        buf.append(" argSize=").append(this.argSize);
        buf.append(" argType=").append(this.argType);
        buf.append(" base=").append(this.base);
        buf.append(" decimalFormatPattern='").append(this.decimalFormatPattern).append("'");

        return buf.append(")").toString();
    }

    public static void main (String[] _args) {
        Object[] args = new Object[] {
            new Object[]{"%2d", new Integer(100)}
            ,new Object[]{"%2d", new Integer(-100)}
            ,new Object[]{"%+2d", new Integer(100)}
            ,new Object[]{"%+2d", new Integer(-100)}
            ,new Object[]{"%5d", new Integer(100)}
            ,new Object[]{"%5d", new Integer(-100)}
            ,new Object[]{"%+5d", new Integer(100)}
            ,new Object[]{"%+5d", new Integer(-100)}
            ,new Object[]{"%.5d", new Integer(100)}
            ,new Object[]{"%.5d", new Integer(-100)}

            ,new Object[]{"%8.2f", new Float(10.01)}
            ,new Object[]{"%8.2f", new Float(-10.01)}
            ,new Object[]{"%+8.2f", new Float(10.01)}
            ,new Object[]{"%+8.2f", new Float(-10.01)}
            ,new Object[]{"%8.2f", new Float(100.01)}
            ,new Object[]{"%8.2f", new Float(-100.01)}

            ,new Object[]{"%5.2f", new Double(3e8)}
            ,new Object[]{"%5.2f", new Double(-3e8)}
            ,new Object[]{"%+5.2f", new Double(3e8)}
            ,new Object[]{"%+5.2f", new Double(-3e8)}
            ,new Object[]{"%5.2e", new Double(3e8)}
            ,new Object[]{"%5.2e", new Double(-3e8)}
            ,new Object[]{"%+5.2e", new Double(3e8)}
            ,new Object[]{"%+5.2e", new Double(-3e8)}
            ,new Object[]{"%5.2E", new Double(3e8)}
            ,new Object[]{"%5.2E", new Double(-3e8)}
            ,new Object[]{"%+5.2E", new Double(3e-8)}
            ,new Object[]{"%+5.2E", new Double(-3e-8)}
        };

        for(int ii=0; ii<args.length ;ii++) {
            Object[] parts = (Object[])args[ii];
            String format = (String)parts[0];
            Number number = (Number)parts[1];

            CRangeCheckedDecimalFormat formatter =  new CRangeCheckedDecimalFormat(format);
            String asStr = formatter.format(number);

            Number asNum = null;
            try {
                asNum = formatter.parse(asStr);
            } catch(java.text.ParseException ex) {
                ex.printStackTrace();
                asNum = null;
            }

            System.out.println(format
                                + "("+ formatter.decimalFormatPattern+"): "
                                + number + " -> " + asStr + " -> " + asNum
                                );
        }
    }
}
