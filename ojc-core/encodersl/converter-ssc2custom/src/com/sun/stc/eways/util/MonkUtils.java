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
 * @(#)MonkUtils.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.eways.util;

import java.util.Map;
import java.util.Date;
import java.util.Random;

import java.text.SimpleDateFormat;
import java.text.NumberFormat;
import java.text.FieldPosition;
import java.text.ParseException;

import java.net.InetAddress;
import java.net.UnknownHostException;

import com.sun.stc.jcsre.FormatterFactory;

/**
 * this clas encapsulates various monk functionalities
 *
 * @author Scott Steadman ssteadman@seebeyond.com
 */
public class MonkUtils {

    /** 
     * String containing result characters for hex conversion
     */
    private static final String hexMap = "0123456789ABCDEF";

    private static final char PCT = '%';
    private static final String INTEGER_ARGTYPES ="doxXzZ]";
    private static final String REAL_ARGTYPES = "eEf";
    private static final String NUMERIC_ARGTYPES = INTEGER_ARGTYPES + REAL_ARGTYPES;
    private static final String STRING_ARGTYPES = "s";
    private static final String VALID_ARGTYPES = NUMERIC_ARGTYPES + STRING_ARGTYPES;

    private static final String UNIQUEID_FORMAT = "yyyyMMddHHmmss'0'S";

    /** 
     * variables for GUID generation
     */
    private static String SYSTEM_ID = null;
    private static long last_time = System.currentTimeMillis();
    private static long sequence = new Random().nextLong() & 0x00FFFFFFFFFFFFFFL;

    /**
     * hidden constructor to prevent instantiation
     */
    protected MonkUtils (){}

    /**
     * generate a unique id 
     *
     * @return a long containing the unique id
     *
     * @see #uniqueID()
     */
    public static long uniqueId () {
        return uniqueID();
    }

    /**
     * generate a unique id.
     *
     * @return a long containing the unique id
     */
    public static long uniqueID () {
        return Long.parseLong(FormatterFactory.getDateFormat(UNIQUEID_FORMAT).format(new Date()));
    }



    /**
     * return true if the pattern is a monk date format
     *
     * @param _pattern  the pattern
     *
     * @return true if the pattern is a monk date format
     */
    public static boolean isMonkDatePattern (String _pattern) {
        return _pattern.indexOf("%")>-1;
    }

    /**
     * convert a monk date pattern into a java date patten
     * suitable for use by <code>java.text.SimpleDateFormat</code>.
     *
     * @param _pattern the monk pattern
     *
     * @return the java pattern or null if _pattern is null.
     *
     * @throws java.text.ParseException if unable to convert it.
     *
     * @see java.text.SimpleDateFormat
     */
    public static String toJavaDatePattern (String _pattern) throws ParseException {

        // quick return if null parameter
        if(null == _pattern) return null;

        StringBuffer buf = new StringBuffer(_pattern.length());

        int len = _pattern.length();
        int start = 0;
        while(start < len) {
            int pos = _pattern.indexOf("%", start);

            // non-formatting characters between start and pos
            if(pos > start) {
                buf.append("'");
                buf.append(StringUtils.expandDups(_pattern.substring(start,pos),"'"));
                buf.append("'");

            // final characters in pattern
            } else if(-1 == pos) {
                buf.append("'");
                buf.append(StringUtils.expandDups(_pattern.substring(start),"'"));
                buf.append("'");
                break;
            }

            char c = _pattern.charAt(++pos);
            switch(c) {
                case 'A': buf.append("EEEE");break;
                case 'a': buf.append("EEE");break;
                case 'B': buf.append("MMMM");break;
                case 'b':
                case 'h': buf.append("MMM");break;
                case 'C': buf.append("EEE MMM d HH:mm:ss yyyy");break;
                case 'c': 
                case 'D': buf.append("MM/dd/yy");break;
                case 'd': buf.append("dd");break;
                case 'e': buf.append("d");break;
                case 'H': buf.append("HH");break;
                case 'I': buf.append("hh");break;
                case 'j': buf.append("DDD");break;
                case 'k': buf.append("H");break;
                case 'l': buf.append("h");break;
                case 'M': buf.append("mm");break;
                case 'm': buf.append("MM");break;
                case 'n': buf.append("\n");break;
                case 'p': buf.append("a");break;
                case 'R': buf.append("HH:mm");break;
                case 'r': buf.append("hh:mm:ss a");break;
                case 't': buf.append("\t");break;
                case 'T': buf.append("HH:mm:ss");break;
                case 'S': 
                case 's': buf.append("ss");break;
                case 'X': buf.append("hh:mm:ss");break;
                case 'U': 
                case 'W': buf.append("ww");break;
                case 'w': buf.append("EEEE");break;
                case 'x': buf.append("MM/dd/yy hh:mm:ss");break;
                case 'Y': buf.append("yyyy");break;
                case 'y': buf.append("yy");break;
                case 'Z': buf.append("z");break;
                case '%': buf.append("%");break;
                default : throw new ParseException("Unknown pattern character: " + c, pos);
            }

            start=++pos;
        }

        return buf.toString();
    }

    /**
     * format a string similar to sprintf
     * 
     * @param _pattern pattern to use
     * @param _items items to format
     *
     * @return formatted string
     */
    public static String sprintf (String _pattern, Object[] _args) {

        StringBuffer buf = new StringBuffer();

        int len = _pattern.length();
        int start = 0;
        int argnum = 0;

        while(start < len && argnum < _args.length) {

            int pos = _pattern.indexOf(PCT, start);

            if(pos > start) buf.append(_pattern.substring(start, pos));

            if(-1 == pos) break;

            // handle %%
            if(PCT == _pattern.charAt(pos+1)) {
                buf.append(PCT);
                start = pos+2;
                continue;
            }

            // find last part of macro
            start = pos;
            while(VALID_ARGTYPES.indexOf(_pattern.charAt(pos)) == -1) {
                pos++;
            }

            String part = _pattern.substring(start, ++pos);

            // string format macro
            if(STRING_ARGTYPES.indexOf(part.charAt(pos-start-1)) == 0) {

                FormatterFactory.getStringFormat(part).format(_args[argnum++], buf, new FieldPosition(0));

            // numeric format macro
            } else {

                FormatterFactory.getNumberFormat(part).format(_args[argnum++], buf, new FieldPosition(0));
            }

            start = pos;
        }

        if(start < len) {
            buf.append(_pattern.substring(start));
        }

        return buf.toString();
    }


    /**
     * format a string similar to sprintf
     * 
     * @param _pattern pattern to use (should only contain %{sign}{width}.{prec}{size}{type})
     * @param _item item to format
     *
     * @return formatted string
     */
    public static String sprintf (String _pattern, Object _arg) {

        StringBuffer buf = new StringBuffer();

        // numeric format 
        if(_arg instanceof Number) {
            FormatterFactory.getNumberFormat(_pattern).format(_arg, buf, new FieldPosition(0));

        // string format
        } else {
            FormatterFactory.getStringFormat(_pattern).format(_arg.toString(), buf, new FieldPosition(0));

        }

        return buf.toString();
    }

    /** 
     * create a StringBuffer with the specified number of 0s in it
     * 
     * @param _count how many zeros
     *
     * @return a StringBuffer with the specified number of zeros
     */
	private static StringBuffer zeros (int _count) {
		StringBuffer sb = new StringBuffer(_count) ;
		for (int i=0; i<_count ;i++) {
			sb.append('0') ;
		}
		return sb ;
	}
	
    /**
     * convert a byte to a hex string
     *
     * @param _b the byte
     *
     * @return a string containing the hex representation of _b
     */
	public static String asHex (byte _b) {
		int j = 2 ;
		StringBuffer r = zeros(j) ;

		while (_b!=0 && --j>=0) {
			r.setCharAt(j, hexMap.charAt(_b & 15)) ;
			_b >>>= 4 ;
		}

		return r.toString() ;
	}

    /**
     * convert a byte array into a list of hex numbers
     *
     * @param _bytes byte array
     * @param _perLine how many bytes to emit before a newline
     *
     * @return a string containing the hex representation of the byte array
     */
    public static String asHex(byte[] _bytes, int _perLine) {
        StringBuffer result = new StringBuffer(_bytes.length<<1);
        for(int ii=0; ii<_bytes.length ;ii++) {

            if(ii != 0 && (ii % _perLine) == 0) result.append('\n');

            result.append(asHex(_bytes[ii]));
            result.append(' ');
        }

        return result.toString().trim();
    }

    /**
     * convert a short to a hex string
     *
     * @param _s the short
     *
     * @return a string containing the hex representation of _s
     */
	public static String asHex (short _s) {
		int j = 4 ;
		StringBuffer r = zeros(j) ;

		while (_s!=0 && --j>=0) {
			r.setCharAt(j, hexMap.charAt(_s & 15)) ;
			_s >>>= 4 ;
		}

		return r.toString() ;
	}
	
    /**
     * convert an integer to a hex string
     *
     * @param _i the integer
     *
     * @return a string containing the hex representation of _i
     */
	public static String asHex (int _i) {
		int j = 8 ;
		StringBuffer r = zeros(j) ;
		
		while (_i!=0 && --j>=0) {
			r.setCharAt(j, hexMap.charAt(_i & 15)) ;
			_i >>>= 4 ;
		}
		
		return r.toString() ;
	}
	
    /**
     * convert an long to a hex string
     *
     * @param _l the long
     *
     * @return a string containing the hex representation of _l
     */
	public static String asHex (long _l) {
		int j = 16 ;
		StringBuffer r = zeros(j);
		
		while (_l != 0 && --j >= 0) {
			r.setCharAt(j, hexMap.charAt((int)(_l & 15))) ;
			_l >>>= 4 ;
		}
		
		return r.toString() ;
	}

    /**
     * Create a string containing the hex representation of the
     * specified byte array.
     *
     * @param _bytes
     *
     * @return string containing hex representation
     */
	public static String toHex (byte[] _bytes) {

		StringBuffer output = new StringBuffer();
		int len = _bytes.length;
		byte b;
		boolean overflow = false;
		StringBuffer chars = new StringBuffer();
		StringBuffer hex = new StringBuffer();

		for(int i = 0; i < len; i++) {
			if(i != 0 && i % 16 == 0) {
				output.append(makeLine(hex, chars));
				hex.setLength(0);
				chars.setLength(0);
				overflow = false;
			}
			else
				overflow = true;

			b = _bytes[i];
			if(b >= 0x20 && b <= 0x7f)
				chars.append((char)b);
			else
				chars.append('.');
			
			hex.append(hexMap.charAt( (b&0xf0) >> 4 ));
			hex.append(hexMap.charAt( (b&0x0f) ));
			hex.append(' ');
		}
		
		if(overflow) {
			output.append(makeLine(hex, chars));
		}
		
		return(output.toString());
	}
    
    /**
     * convert the string to the hexadecimal representation
     * of the bytes of its characters
     *
     * @param _str string to convrt
     *
     * @return the hex representation of the bytes that make up the character
     */
	public static String toHex (String _str) {
		return(toHex(_str.getBytes()));
	}
    
    /**
     * format a line for hex dump
     *
     * @param _hex the hexadecimal portion of the line
     * @param _chars the character portion of the line
     *
     * @return a string that contains the formatted line
     */
	private static String makeLine (StringBuffer _hex, StringBuffer _chars) {
		final String spaces = "                                                ";
		return(_hex + "    " + spaces.substring(0, 48 - _hex.length()) + _chars) + "\n";
	}

    /**
     * Generate a timestamp string with the timestamp at
     * a specified offset, and the string truncated to a
     * specified length.
     * 
     * @param _offset starting point of string
     * @param _trunc length to truncate string to (-1 means no truncation)
     * @param _pattern pattern to use for timestamp
     *
     * @returns the timestamp string.
     */
    public static String timeStamp (int _offset, int _trunc, String _pattern) {
        return doOffsetTrunc(_offset, _trunc, DateUtils.timeStamp(_pattern));
    }

    /**
     * Generate a uniqe id string with the id at
     * a specified offset, and the string truncated to a
     * specified length.
     * 
     * @param _offset starting point of string
     * @param _trunc length to truncate string to (-1 means no truncation)
     *
     * @returns the unique id string.
     */
    public static String uniqueId (int _offset, int _trunc) {
        return doOffsetTrunc(_offset, _trunc, ""+uniqueId());
    }

    /**
     * return a globally unique user id similar to Monk's
     *
     * @returns a globally unique user id
     */
     public static synchronized String getGUID() {
        StringBuffer buf = new StringBuffer(38);

        // create SYSTEM_ID if necessary
        if(null == SYSTEM_ID) {
            try {
                byte[] bytes = InetAddress.getLocalHost().getAddress();
                for(int ii=0; ii<bytes.length ;ii++) {
                    buf.append(asHex(bytes[ii]));
                }
            } catch(UnknownHostException ex) {
                ex.printStackTrace();
            }

            buf.append(asHex((short)(new Random().nextInt())));

            SYSTEM_ID = buf.toString();
            buf.setLength(0);
        }

        long now = System.currentTimeMillis();
        if(now == MonkUtils.last_time) {
            MonkUtils.sequence++;
            MonkUtils.sequence &= 0x00FFFFFFFFFFFFFFL;
        }
        MonkUtils.last_time = now;

        byte[] bytes = STCTypeConverter.toByteArray(now);

        buf.append("{");

        buf.append(asHex(bytes[7]));
        buf.append(asHex(bytes[6]));
        buf.append(asHex(bytes[5]));
        buf.append(asHex(bytes[4]));
        buf.append('-');

        bytes = STCTypeConverter.toByteArray(sequence);
        buf.append(asHex(bytes[7]));
        buf.append(asHex(bytes[6]));
        buf.append('-');
        buf.append(asHex(bytes[5]));
        buf.append(asHex(bytes[4]));
        buf.append('-');
        buf.append(asHex(bytes[3]));
        buf.append(asHex(bytes[2]));
        
        buf.append('-');
        buf.append(SYSTEM_ID);

        buf.append('}');

        return buf.toString();
     }

    /**
     * return a string that contains another string starting
     * at the specified offset and truncated to a specified
     * length.
     *
     * @param _offset starting point of string
     * @param _trunc length to truncate string to (-1 means no truncation)
     * @param _str string to use
     *
     * @returns the offset and truncated.
     */
    public static String doOffsetTrunc (int _offset, int _trunc, String _str) {
        
        // quick return if no further processing necessary
        if(0==_offset && -1==_trunc) return _str;
        if(0==_trunc) return "";

        StringBuffer buffer = new StringBuffer(_str.length()<<1);

        for(int ii=0; ii<_offset ;ii++) buffer.append(' ');

        buffer.append(_str);

        if(_trunc>0) {
            while(buffer.length()<_trunc) {
                buffer.append(' ');
            }
            buffer.setLength(_trunc);
        }

        return buffer.toString();
    }

    /**
     * Monk compatability function to
     * copy a string value to a pattern while doing truncation and shifting.
     *
     * @param _src the string value
     * @param _srcOffset offset in _value to start at
     * @param _srcLen number of characters to use (-1 == all characters)
     * @param _destOffset where to start string in the result
     * @param _destLen number of characters to emit (-1 == all characters)
     * @param _pattern pattern to use to format data (can be null for straight copy)
     * @param _toPrepend string to prepend to result
     * @param _strip strip trailing spaces
     *
     * @return the result
     */
    public static String copy (String _src
                            ,int _srcOffset
                            ,int _srcLen
                            ,int _destOffset
                            ,int _destLen
                            ,String _pattern
                            ,String _toPrepend
                            ,boolean _strip
    ) {
        return copy(_src, _srcOffset, _srcLen, _destOffset, _destLen, _pattern, _toPrepend, _strip?" ":"");
    }

    /**
     * Monk compatability function to
     * copy a string value to a pattern while doing truncation and shifting.
     *
     * @param _src the string value
     * @param _srcOffset offset in _value to start at
     * @param _srcLen number of characters to use (-1 == all characters)
     * @param _destOffset where to start string in the result
     * @param _destLen number of characters to emit (-1 == all characters)
     * @param _pattern pattern to use to format data (can be null for straight copy)
     * @param _toPrepend string to prepend to result
     * @param _trimChars trailing characters to trim
     *
     * @return the result
     */
    public static String copy (String _src
                            ,int _srcOffset
                            ,int _srcLen
                            ,int _destOffset
                            ,int _destLen
                            ,String _pattern
                            ,String _toPrepend
                            ,String _trimChars
    ) {

        // get substring 
        if(_srcLen >= 0) {
            _src = _src.substring(_srcOffset, _srcOffset+_srcLen);
        } else if(_srcOffset > 0) {
            _src = _src.substring(_srcOffset);
        }

        if(!StringUtils.empty(_pattern)) {
            char argType = _pattern.charAt(_pattern.length()-1);

            if(INTEGER_ARGTYPES.indexOf(argType) > -1) {
                _src = sprintf(_pattern, new Long((long)Double.parseDouble(_src)));
            } else if(REAL_ARGTYPES.indexOf(argType) > -1) {
                _src = sprintf(_pattern, new Double(_src));
            } else {
                _src = sprintf(_pattern, _src);
            }

        }

        if(!StringUtils.empty(_toPrepend)) {
            _src = _toPrepend + _src;
        } else {
            _src = doOffsetTrunc(_destOffset, _destLen, _src);
        }

        if(!StringUtils.empty(_trimChars)) {
            _src = StringUtils.trimRight(_src, _trimChars);
        }

        return _src;
    }
    
    /**
     * Monk compatability function to do a lookup. 
     *
     * @param _srcOffset offset in _value to start at
     * @param _srcLen number of characters to use (-1 == all characters)
     * @param _destOffset where to start string in the result
     * @param _destLen number of characters to emit (-1 == all characters)
     * @param _map a , and | separated list of map elements.
     * @param _default the value to return if the value is not found in the map
     * @param _key key to lookup in datamap
     * @param _pattern pattern to use to format result of datamap
     * @param _trimChars characters to trim off end of string
     *
     * @return the result
     */
    public static String lookup (int _srcOffset
                                ,int _srcLen
                                ,int _destOffset
                                ,int _destLen
                                ,String _map
                                ,String _default
                                ,String _key
                                ,String _pattern
                                ,String _trimChars
    ) {

        // get substring 
        if(_srcLen >= 0) {
            _key = _key.substring(_srcOffset, _srcOffset+_srcLen);
        } else if(_srcOffset > 0) {
            _key = _key.substring(_srcOffset);
        }

        // trim characters
        _key = StringUtils.trimBoth(_key, _trimChars);

        Map theMap = MapUtils.parseMap(_map, ",", "|");
        String result = MapUtils.doMap(theMap, _key);
        if(null == result) result = _default;

        return MonkUtils.copy(result, 0, -1, _destOffset, _destLen, _pattern, null, null);
    }

    public static void main (String[] _args) {
        
        for(int ii=0; ii<_args.length ;ii++) {
            try {

                System.out.println();

                if(_args[ii].equals("-d")) {
                    String pattern = _args[++ii];
                    String javaPattern=MonkUtils.toJavaDatePattern(pattern);
                    SimpleDateFormat fmt = new SimpleDateFormat(javaPattern);
                    System.out.println(pattern + " -> " + javaPattern + " -> " + fmt.format(new Date()));

                } else if(_args[ii].equals("-co")) {
                    String src = _args[++ii];
                    int srcOffset = Integer.parseInt(_args[++ii]);
                    int srcLen = Integer.parseInt(_args[++ii]);
                    int destOffset = Integer.parseInt(_args[++ii]);
                    int destLen = Integer.parseInt(_args[++ii]);
                    String pattern = _args[++ii];
                    boolean strip = "true".equals(_args[++ii]);
                    System.out.println("reference: [01234567890123456789]");
                    System.out.println("     copy: [" + MonkUtils.copy(src, srcOffset, srcLen, destOffset, destLen, pattern, null, strip)+"]");

                } else if(_args[ii].equals("-ts")) {
                    String pattern = _args[++ii];
                    int offset = Integer.parseInt(_args[++ii]);
                    int trunc = Integer.parseInt(_args[++ii]);
                    System.out.println("reference: [01234567890123456789]");
                    System.out.println("timestamp: ["+timeStamp(offset, trunc, pattern)+"]");

                } else if(_args[ii].equals("-n")) {
                    String pattern = _args[++ii];
                    double value = Double.parseDouble(_args[++ii]);
                    NumberFormat fmt = FormatterFactory.getNumberFormat(pattern);
                    System.out.println(pattern + " -> " + fmt.format(value));

                } else if(_args[ii].equals("-u")) {
                    int offset = Integer.parseInt(_args[++ii]);
                    int trunc = Integer.parseInt(_args[++ii]);
                    if(uniqueId() == uniqueId()) {
                        System.err.println("ids not unique across rapid generation.");
                    }
                    System.out.println("reference: [01234567890123456789]");
                    System.out.println("unique id: ["+uniqueId(offset, trunc)+"]");

                } else if(_args[ii].equals("-s")) {
                    String pattern = "\nstr(%%10s): '%10s'"
                                    +"\nstr(%%-10s): '%-10s'"
                                    +"\nstr(%%^10s): '%^10s'"
                                    +"\nstr(%%.3s): '%.3s'"
                                    +"\nstr(%%-.3s): '%-.3s'"
                                    +"\nint(%%2d): '%2d'"
                                    +"\nint(%%.5d): '%.5d'"
                                    +"\noctal(%%3o): '%3o'"
                                    +"\nhex(%%4x): '%4x'"
                                    +"\nfloat(%%2.4f): '%2.4f'"
                                    +"\ndouble(%%+4.2lE): '%+4.2lE'"
                                    +"\ndouble(%%+4.2le): '%+4.2le'"
                                    +"\ndouble(%%+4.2lE): '%+4.2lE'"
                                    +"\ndouble(%%+4.2le): '%+4.2le'"
                                    +"\ntail";

                    Object[] args = new Object[] {
                        new String("test")
                        ,new String("test")
                        ,new String("test")
                        ,new String("testing")
                        ,new String("testing")
                        ,new Integer(100)
                        ,new Integer(100)
                        ,new Long(100)
                        ,new Long(100)
                        ,new Float(3.14159)
                        ,new Double(123.45E-10)
                        ,new Double(123.45E-10)
                        ,new Double(123.45E10)
                        ,new Double(123.45E10)
                    };

                    System.out.println("sprintf: " + sprintf(pattern, args));

            
                } else if(_args[ii].equals("-qa")) {
                    QA(_args[++ii], true);
                    
                } else if(_args[ii].equals("-qas")) {
                    QA(_args[++ii], false);
                    
                } else if(_args[ii].equals("-h")) {
                    usage();
                }


            } catch(Exception ex) {
                ex.printStackTrace();
            }

        }

    }

    /**
     * do QA tests in specified file
     *
     * @param _fileName the name of the file
     * @param _longFormat whether successes should be emitted as well
     *
     * @throws IOException unable to read file
     */
    private static void QA (String _fileName, boolean _longFormat) throws java.io.IOException {

        String lines = FileUtils.readString(_fileName);
        java.util.StringTokenizer lineTkzr = new java.util.StringTokenizer(lines,System.getProperty("line.separator"));
        while(lineTkzr.hasMoreTokens()) {

            String line = lineTkzr.nextToken();
            if(line.startsWith("#")) continue;

            java.util.StringTokenizer tkzr = new java.util.StringTokenizer(line,",");
            String pattern = tkzr.nextToken();
            String value = tkzr.nextToken();
            String expected = tkzr.nextToken();

            String result = null;
            char lastChar = pattern.charAt(pattern.length()-1);

            // integer type
            if(INTEGER_ARGTYPES.indexOf(lastChar) > -1) {
                result = FormatterFactory.getNumberFormat(pattern).format(Double.parseDouble(value));

            // real type
            } else if(REAL_ARGTYPES.indexOf(lastChar) > -1) {
                result = FormatterFactory.getNumberFormat(pattern).format(Double.parseDouble(value));

            // string argtype
            } else {
                result = FormatterFactory.getStringFormat(pattern).format(value);
            }

            // generate output
            if(_longFormat || !result.equals(expected)) {
                System.out.print("sprintf(\""+pattern+"\", "+value+") =>");
                if(value.length()+pattern.length()<8) System.out.print("\t");
                if(value.length()+pattern.length()<16) System.out.print("\t");
                if(value.length()+pattern.length()<24) System.out.print("\t");
                System.out.print("\t["+result+"]");
                if(!result.equals(expected)) System.out.print(" **** ["+expected+"]");
                System.out.println();
            }

        }
    }

    private static void usage() {
        System.out.println();
        System.out.println("Usage is: MonkUtils <option>");
        System.out.println();
        System.out.println("Options are:");
        System.out.println("\t-d <pattern>\t\tDisplays current date using pattern.");
        System.out.println("\t-n <pattern> <number>\tFormats number using pattern.");
        System.out.println("\t-u\t\t\tGenerates a unique id.");
        System.out.println("\t-qa <filename>\t\ttests string/number formatting for all entries in a file.");
        System.out.println();
        System.out.println();
    }

    public static String convertFileSeparators(String aString)
    {
      String result = aString;
      
      if (aString != null && aString.length() > 0)
      {
        if (java.io.File.separatorChar == '\\')
        {
          if (aString.indexOf((int) '/') >= 0)
          {
            result = aString.replace('/', java.io.File.separatorChar);
          }
        }      
      
        if (java.io.File.separatorChar == '/')
        {
          if (aString.indexOf((int) '\\') >= 0)
          {
            result = aString.replace('\\', java.io.File.separatorChar);
          }
        }
      }
      return result;
    }
}
