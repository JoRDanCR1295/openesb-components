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
 * @(#)StringCoderUtil.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.io.InputStream;
import java.io.FileInputStream;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.UnsupportedEncodingException;

/**
 * This is a utility class to help with manipulating string
 * encodings
 */
public class StringCoderUtil {

    /**
     * verbose flag
     */
    private static boolean verboseFlag = false;

    /**
     * hidden constructor for now
     */
    private StringCoderUtil(){}

    /**
     * utility method to support translation of encodings
     *
     * @param _src the byte array to translate
     * @param _fromEnc the encoding to translate from
     * @param _toEnc the encoding to translate to
     *
     * @return the translated byte array
     *
     * @throws IllegalArgument exception if any argument is null
     * @throws UnsupportedEncodingException _fromEnc or _toEnc not supported
     */
    public static byte[] translate(byte[] _src, String _fromEnc, String _toEnc)
            throws IllegalArgumentException, UnsupportedEncodingException
    {
        if(null == _src) throw new IllegalArgumentException("_src cannot be null.");
        if(null == _fromEnc) throw new IllegalArgumentException("_fromEnc cannot be null.");
        if(null == _toEnc) throw new IllegalArgumentException("_toEnc cannot be null.");

        IStringCoder decoder = StringCoderFactory.getStringCoder(_fromEnc);
        IStringCoder encoder = StringCoderFactory.getStringCoder(_toEnc);
        return encoder.encode(decoder.decode(_src));
    }

    /**
     * convert a byte array to a string containing the hex
     * representation of the bytes separated by a spaces.
     *
     * @param _b the array
     *
     * @return a string containing the hex bytes separated by spaces
     */
    public static String toString(byte[] _b) {
        StringBuffer sb = new StringBuffer(_b.length);
        for(int ii=0; ii<_b.length ;ii++) {
            sb.append("0123456789ABCDEF".charAt((_b[ii]>>>4)&0x0F));
            sb.append("0123456789ABCDEF".charAt(_b[ii]&0x0F));
            sb.append(" ");
        }
        return sb.toString();
    }

    /**
     * unit testing method
     */
    public static void main(String[] _args) {

        try {

            // parse arguments
            byte[] input = null;
            String fromEnc = null;
            String toEnc = null;
            String inFile = null;
            String outFile = null;
            for(int ii=0; ii<_args.length ;ii++) {
                String arg=_args[ii];

                if("-i".equals(arg)) {
                    inFile = _args[++ii];
                } else if("-o".equals(arg)) {
                    outFile = _args[++ii];
                } else if("-f".equals(arg)) {
                    fromEnc = _args[++ii];
                } else if("-t".equals(arg)) {
                    toEnc = _args[++ii];
                } else if("-v".equals(arg)) {
                    StringCoderUtil.verboseFlag = true;
                } else {
                    System.err.println("Unknown option: " + arg);
                    System.exit(1);
                }
            }

            if(null==inFile) throw new IllegalArgumentException("input file not specified.");
            if(null==fromEnc) throw new IllegalArgumentException("source encoding not specified.");
            if(null==toEnc) throw new IllegalArgumentException("destination encoding not specified.");

            // read input file
            verbose("reading " + inFile + "...");
            InputStream is = new FileInputStream(inFile);
            byte[] fromBytes = new byte[is.available()];
            is.read(fromBytes);
            is.close();
            is=null;
            verbose(fromEnc + ": " + StringCoderUtil.toString(fromBytes));

            // translate bytes
            verbose("translating...");
            byte[] toBytes = StringCoderUtil.translate(fromBytes, fromEnc, toEnc);
            verbose(toEnc + ": " + StringCoderUtil.toString(toBytes));

            // write output file
            OutputStream os = null;
            if(null!=outFile) {
                verbose("writing " + outFile + "...");
                os = new FileOutputStream(outFile);
            } else {
                os = System.out;
            }
            os.write(toBytes);
            os.flush();
            os.close();
            os=null;


        } catch(Exception ex) {
            ex.printStackTrace();
            if(ex instanceof IllegalArgumentException) usage();
        }

    }

    /**
     * print usage statement to System.out
     */
    private static void usage() {
        System.out.println();
        System.out.println("Usage is: " + StringCoderUtil.class.getName() + " [options]");
        System.out.println();
        System.out.println("Options are:");
        System.out.println("\t-i <file>\tspecify the input file to read.");
        System.out.println("\t-o <file>\tspecify the output file to write.");
        System.out.println("\t-f <encoding>\tspecify the encoding of the input file.");
        System.out.println("\t-t <encoding>\tspecify the encoding of the output.");
        System.out.println("\t-v\t\tdump translation information to the screen.");
        System.out.println();
    }

    /**
     * print verbose text
     */
    private static void verbose(String _str) {
        if(StringCoderUtil.verboseFlag) System.err.println(_str);
    }

}
