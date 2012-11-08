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
 * @(#)StringCoderFactory.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.util.Map;
import java.util.HashMap;
import java.util.Arrays;
import java.io.UnsupportedEncodingException;

import com.sun.stc.jcsre.ssc.StringFile8;
import com.sun.stc.jcsre.ssc.StringFileCoder;

/**
 * This is a factory class used to create and cache String coders.
 *
 * Users can add coders to the factory manually by calling the
 * <code>registerCoder(String, IStringCoder)</code> method.o
 * 
 * Since coders are stateless, they are cached within the factory.
 * 
 * To request a coder the user should call the <code>getCoder(String)</code>
 * method.  Subsequent calls to this method with the same string will return
 * the same coder instance.
 *
 * @see StringCoderUtil
 */
public class StringCoderFactory {

    /**
     * flag to activate unit testing
     * @see #main(String[])
     */
    private static final boolean unitTest = Boolean.getBoolean("com.sun.stc.jcsre.StringCoderFactory.unitTest");

    private static Map cache = new HashMap();

    // This initializer can be used to inialize internal aliases
    static {
        try {

            // register aliases
            StringCoderFactory.registerCoder("UHC"
                    , StringCoderFactory.getStringCoder("MS949")
            );

        } catch(Exception ex) {
            System.err.println("Error in StringCoderFactory static initializer.");
            ex.printStackTrace();
        }
    }

    /**
     * private constructor to prevent instantiation
     */
    private StringCoderFactory(){}
    
    
    /**
     * Calculates a per-thread key for the encoder.
     *
     * This method must be used if the encoder you are registering
     * is not thread safe.  Basically you call this method with the
     * encoding you are going to override, and it returns a unique
     * per-thread key for the encoder
     *
     * @param _enc the encoding.
     * @return the coder registration key.
     */
    private static String getPerThreadKey(String _enc) {
        return (Thread.currentThread().hashCode()+"-"+_enc);
    }

    /**
     * register an encoder with the factory.
     * 
     * This method will override the previous encoder.
     *
     * @param _enc the encoding
     * @param _coder the coder
     */
    public static void registerCoder(String _enc, IStringCoder _coder) {
        StringCoderFactory.cache.put(_enc, _coder);
    }

    /**
     * return an IStringcoder that will do conversion to and
     * from specified encoding.
     *
     * @param _enc encoding
     *
     * @returns an IStringCoder instance.
     *
     * @throws UnsupportedEncodingException encoding not supported
     */
    public static IStringCoder getStringCoder(String _enc)
        throws UnsupportedEncodingException
    {
        String key = getPerThreadKey(_enc);
        IStringCoder coder = (IStringCoder)StringCoderFactory.cache.get(key);
        if(null == coder) {

            // try StringFileCoder
            try {
                coder = new StringFileCoder(_enc);
            // try next coder
            } catch(UnsupportedEncodingException ex) {
                coder = null;
            }

            // try StringFile8 coder
            if(null == coder) {
                try {
                    coder = new StringFile8(_enc);
                // try next coder
                } catch(UnsupportedEncodingException ex) {
                    coder = null;
                }
            }

            if(null == coder) {
                coder = new StringCoderImpl(_enc);
            }
            StringCoderFactory.cache.put(key, coder);
        }
        return coder;
    }

    /**
     * unit testing method
     */
    public static void main(String[] _args) {
        if(!StringCoderFactory.unitTest) return;

        try {

            //
            // validate caching
            //
            IStringCoder first = StringCoderFactory.getStringCoder("UTF8");
            IStringCoder second = StringCoderFactory.getStringCoder("UTF8");
            if(first == second) {
                System.out.println("cache test passed.");
            } else {
                throw new Exception("cache test failed.");
            }
            IStringCoder third = StringCoderFactory.getStringCoder("UHC");
            if(third != second) {
                System.out.println("cache test passed.");
            } else {
                throw new Exception("cache test failed.");
            }

            //
            // validate translate
            //
            final String source = "This is a test \u2304.";
            System.out.println("original: " + source);
            byte[] utf8 = first.encode(source);
            System.out.print("utf8: " + StringCoderUtil.toString(utf8));
            byte[] uhc = third.encode(source);
            System.out.print("uhc: " + StringCoderUtil.toString(uhc));
            byte[] tran = StringCoderUtil.translate(utf8, "UTF8", "UHC");
            System.out.print("tran: " + StringCoderUtil.toString(tran));
            if(Arrays.equals(tran, uhc)) {
                System.out.println("translation passed.");
            } else {
                throw new Exception("translation failed.");
            }

        } catch(Exception ex) {
            ex.printStackTrace();
        }

    }

}
