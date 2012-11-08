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
 * @(#)StringFileCoder.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre.ssc;

import java.io.Serializable;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;

import java.util.StringTokenizer;

import com.sun.stc.jcsre.IStringCoder;
import com.sun.stc.jcsre.JCSProperties;
import com.sun.stc.jcsre.PropertyException;
import com.sun.stc.jcsre.StringCoderUtil;

/**
 * An implementation of the string coder interface, that takes its encoding
 * from a table in a text file.
 *
 * The encoding files are all sought relative to the directory provided
 * by the JCS property "StringFileCoder.dir".  The name of the file in
 * the directory must match the encoding parameter passed to the constructor
 * for this class.  No extension will be added to the specified encoding to
 * derive the filename.
 * 
 * Encoding files consist of line of the following format:
 *
 *   [ <multi-byte value> <Unicode value> ] [ "#" comment ]
 *
 * In other words a multi-byte value (hexadecimal) is equated with a 
 * Unicode value in hexadecimal.  If the unicode value is blank, the
 * byte is unmapped.
 *
 * When the internal mapping array is created, it maps byte to byte.
 * This means that straight byte mappings (ie 0x00 - 0x7F) can be
 * omitted from the input file to save space.
 *
 * Array initialization can be overridden in decendants by changing
 * the <code>initializeMaps</code> method.
 * @see #initializeMaps()
 * 
 */
public class StringFileCoder implements IStringCoder {

    /**
     * character used to indicate a comment (default #)
     */
    public static final char COMMENT_CHAR = '#';

    /**
     * character used to indicate no mapping
     */
    public static final char NO_MAPPING = '\uFFFF';

    /**
     * extension used on cache file
     */
    public static final String CACHE_EXT = ".cache";


    private String encoding = null;
    private String mapDir = JCSProperties.getProperty("StringFileCoder.mapDir");
    private String cacheDir = JCSProperties.getProperty("StringFileCoder.cacheDir");

    /** 
     * array used to convert a unicode character to a byte array
     */
    private byte[][] u2b = null;

    /**
     * array used to convert a byte array to a unicode character
     */
    private b2uNode[] b2u = null;

    /**
     * maximum number of bytes used to represent a unicode character
     */
    private int maxBytesPerChar=1;

        
    private static StringFileCoder utf8 = null;
    /**
     * Creates an instance from encoding name and optionally
     * caches internal state for quicker instantiation on
     * future constructions.
     *
     * @param _enc name of encoding
     * @param _cache whether to cache the internal arrays on disk.
     *
     * @throws UnsupportedEncodingException encoding not supported
     */
    public StringFileCoder (String _enc, boolean _cache)
        throws UnsupportedEncodingException
    {

        // quick return if encoding is null
	if (_enc == null) {
	    throw new UnsupportedEncodingException("null encoding name");
        }

	this.encoding = _enc;
        
        if (_enc != null && (_enc.equalsIgnoreCase("UTF-8") || _enc.equalsIgnoreCase("UTF8")) && utf8 != null)
        {
            this.u2b = utf8.u2b;
            this.b2u = utf8.b2u;
            return;
        }

        // there is no cache file
        if(!readCache(_enc)) {

            // parse the encoding file 
            parseEncodingFile(_enc);

            // write cache if requested
            if(_cache) writeCache(_enc);
        }

        if (_enc.equalsIgnoreCase("UTF-8") || _enc.equalsIgnoreCase("UTF8"))
            utf8 = this;
    }

    /**
     * creates an instance from an encoding name.
     *
     * @param _enc name of encoding
     *
     * @throws UnsupportedEncodingException encoding not supported
     */
    public StringFileCoder (String _enc) throws UnsupportedEncodingException {
        this(_enc, false);
    }

    /**
     * Read cache file into instance.
     *
     * This method prepends mapDir and appends CACHE_EXT to determine
     * the full path to the cache file.
     *
     * @param _enc cache file to read
     */
    private boolean readCache(String _enc) {

        // read regular file
        File file = null;
        _enc = _enc.concat(CACHE_EXT);
        if(null!=cacheDir) {
            file = new File(cacheDir, _enc);
        } else {
            file = new File(_enc);
        }

        if(!file.exists()) return false;

        try {
            ObjectInputStream ois = new ObjectInputStream(new FileInputStream(file));

            u2b = (byte[][])ois.readObject();
            b2u = (b2uNode[])ois.readObject();
            maxBytesPerChar = ((Integer)ois.readObject()).intValue();
            ois.close();

            ois = null;
        } catch(Exception ex) {
            ex.printStackTrace();
            return false;
        }

        return true;
    }

    /**
     * Write instance into cache file.
     *
     * This method prepends mapDir and appends CACHE_EXT to determine
     * the full path to the cache file.
     *
     * @param _enc cache file to read
     */
    private void writeCache(String _enc) {
        
        // read regular file
        File file = null;
        _enc = _enc.concat(CACHE_EXT);
        if(null!=cacheDir) {
            file = new File(cacheDir, _enc);
        } else {
            file = new File(_enc);
        }

        try {
            ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(file));   

            oos.writeObject(u2b);
            oos.writeObject(b2u);
            oos.writeObject(new Integer(maxBytesPerChar));
            oos.close();

            oos = null;
        } catch(Exception ex) {
            ex.printStackTrace();
            System.err.println("*** Warning Unable to write cache file: "
                + ex.getMessage()
            );
        }
    }

    /**
     * parse a file of encodings into internal arrays.
     *
     * This method prepends mapDir to determine
     * the full path to the encoding file.
     *
     * @param _enc the file to read.
     *
     * @throws UnsupportedEncodingException encoding not supported
     */
    private void parseEncodingFile(String _enc)
        throws UnsupportedEncodingException
    {

        // read regular file
        File file = null;
        if(null!=mapDir) {
            file = new File(mapDir, _enc);
        } else {
            file = new File(_enc);
        }

	LineNumberReader in = null;
	try {
	    // Try to open the file.
	    in = new LineNumberReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
	} catch (UnsupportedEncodingException u) {
	    throw new UnsupportedEncodingException("platform has no UTF-8?");
	} catch (FileNotFoundException f) {
	    throw new UnsupportedEncodingException("no code file [" + file.getPath() + ']');
	}

        initializeMaps();

	for (;;) {

            String line = null;
	    try {
                line = in.readLine();
            } catch(IOException io) {
		throw new UnsupportedEncodingException(
		    "error reading code file [" + file.getPath() + "]: "
                    + io.getMessage()
                );
	    }

	    if(null == line) break;

            // Strip comment.
	    int pos = line.indexOf(COMMENT_CHAR);
	    if(pos >= 0) line = line.substring(0, pos);

            // Skip empty lines.
	    if("".equals(line)) continue;

	    StringTokenizer st = new StringTokenizer(line);

            // first get byte array
            byte[] bytes = null;
	    if(st.hasMoreTokens()) {

                String tok = st.nextToken();
		try {

                    long l = 0L;
                    if(tok.startsWith("0x")) {
                        l = Long.parseLong(tok.substring(2), 16);
                    } else {
                        l = Long.parseLong(tok);
                    }
                    bytes = toByteArray(l);

                    // adjust maxBytesPerChar
                    if(bytes.length>this.maxBytesPerChar) {
                        this.maxBytesPerChar = bytes.length;
                    }

		} catch (NumberFormatException n) {
		    throw new UnsupportedEncodingException(
			"code file [" + file.getPath() + "], line "
                        + in.getLineNumber()
                        + ": 1st part not a number [" + tok + "]"
                    );
		}

            }

            // then get unicode
            if(st.hasMoreTokens()) {

                String tok = st.nextToken();
                try {
                    char c = 0;
                    if(tok.startsWith("0x")) {
                        c = (char)Long.parseLong(tok.substring(2), 16);
                    } else {
                        c = (char)Long.parseLong(tok);
                    }

                    u2b[c] = bytes;
                    setB2U(bytes, c);


                } catch(NumberFormatException n) {
                    throw new UnsupportedEncodingException(
                        "code file [" + file.getPath() + "], line " +
                        in.getLineNumber() +
                        ": 2nd part not a number [" + tok + "]");
                }

            // no unicode specified
	    } else {
                // clearn b2u
                setB2U(bytes, NO_MAPPING);
            }
            
	}

	try {
            in.close();
        } catch(IOException io) {
	    throw new UnsupportedEncodingException(
		"error closing code file [" + file.getPath() + "]: "
                + io.getMessage()
            );
	}

    }

    /**
     * recurse down into b2u array to get final byte array
     *
     * @param _indices byte array containing indices
     * @param _value character to set last element to
     */
    private void setB2U(byte[] _indices, char _value) {

        int index = (_indices[0] & 0xFF);

        // handle quickie special case
        if(1==_indices.length) {
            b2u[index] = new b2uNode(_value);
            return;
        }

        // create entry if needed
        if(null==b2u[index]) b2u[index] = new b2uNode();

        // recurse down
        b2u[index].set(_indices, 1, _value);

    }


    /**
     * Initialize internal maps.
     *
     * This method initializes internal arrays to be a straight byte to byte
     * mapping.
     *
     * The purpose of this method is to allow array initialization to be
     * overridden in decendant classes.
     */
    protected void initializeMaps() {
	u2b = new byte[0x10000][1];
        for(int ii=0; ii<u2b.length ;ii++) u2b[ii][0] = (byte)(ii&0xFF);

	b2u = new b2uNode[0x100];
        for(int ii=0; ii<b2u.length ;ii++) new b2uNode((char)ii);
    }

    /**
     * Converts a string into a byte array.
     *
     * @param s a string, or null
     * @return byte array containing the encoded string
     * @throws IllegalArgumentException if s can't be converted
     */
    public byte[] encode (String s) throws IllegalArgumentException {

	if (null == s) return null;

	int len = s.length();
	if (len == 0) return new byte[0];

	byte[] buf = new byte[len * this.maxBytesPerChar];
        int bufPos = 0;
        char[] chars = s.toCharArray();
        for(int ii=0; ii<chars.length ;ii++) {
            byte[] bytes = u2b[chars[ii]];
            System.arraycopy(bytes, 0, buf, bufPos, bytes.length);
            bufPos += bytes.length;
        }

        byte[] ret = new byte[bufPos];
        System.arraycopy(buf, 0, ret, 0, bufPos);
        buf = null;
	return ret;
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
    public String decode (byte[] b) throws IllegalArgumentException {

        // handle quickie cases
	if (b == null) {return null;}
	if (b.length == 0) {return "";}

	char[] buf = new char[b.length];
        int bufPos = 0;
        
        // loop through buffer 
        for(int bPos=0; bPos<b.length ;) {
            int curByte = (b[bPos++] & 0xFF);

            // start at root node
            b2uNode curNode = b2u[curByte];

            // move down as necessary
            while(null != curNode.next) {
                curByte = b[bPos++] & 0xFF;
                curNode = curNode.next[curByte];
            }

            buf[bufPos++] = curNode.character;
        }

	return new String(buf, 0, bufPos);
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
	throw new PropertyException(getClass().getName()
            + " has no [" + name + "] property"
        );
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
    public Object getProperty (String name) throws PropertyException {
	throw new IllegalArgumentException(getClass().getName()
            + " has no [" + name + "] property"
        );
    }

    /**
     * convert an integer into a minimal length byte array
     *
     * @param _i the integer
     *
     * @return a minimal length byte array containing the bytes in _i
     */
    private static byte[] toByteArray(long _i) {

        // quick return on zero value
        if(0 == _i) return new byte[1];

        byte[] bytes = null;

        // loop through bytes (8 bytes per long)
        for(int count=7; count>=0 ;count--) {

            // get series of 8 bits from _i
            byte b = (byte)((_i>>>(count<<3))&0xFF);

            // first non-zero byte allocate array
            if(null == bytes && 0 != b) {
                bytes = new byte[count+1];
            }

            // array has been allocated
            if(null != bytes) {
                bytes[bytes.length-count-1] = b;
            }

        }

        return bytes;
    }

    /**
     * inner class used to help map byte arrays to unicode characters
     */
    private static class b2uNode implements Serializable {
        char character = '\uFFFF';
        b2uNode[] next = null;

        b2uNode(){};
        b2uNode(char _c) {this.character = _c;}
        b2uNode(b2uNode[] _n) {this.next = _n;}

        /**
         * recursive method to set the value based on a series of index values
         */
        void set(byte[] _indices, int _pos, char _value) {

            // create next if necessary
            if(null == this.next) this.next = new b2uNode[0x100];

            // at last index
            int index = (_indices[_pos++] & 0xFF);
            if(_pos == _indices.length) {
                this.next[index] = new b2uNode(_value);
                return;
            }

            // create indexed element if necessary
            if(null==this.next[index]) this.next[index] = new b2uNode();

            // recurse into next
            this.next[index].set(_indices, _pos, _value);

        }

    }


    /**
     * File testing method.
     * Usage: main <file> <string>
     * Will try to convert given string back and forth using given encoding,
     * and report an error if this fails to produce the same result.
     *
     * @param _args  command line arguments
     */
    public static void main (String[] _args) {
	try {
	    StringFileCoder coder = new StringFileCoder(_args[0], true);
	    String u1 = _args[1] + " \uCDD2";
            System.out.println("Original: " + u1);
	    byte[] ba = coder.encode(u1);
            System.out.println(" Encoded: " + StringCoderUtil.toString(ba));
	    String u2 = coder.decode(ba);
            System.out.println(" Decoded: " + u2);
	    if (!u1.equals(u2))
		throw new RuntimeException("encode differs from decode");
	} catch (Exception ex) {
	    ex.printStackTrace();
	}
    }

}
