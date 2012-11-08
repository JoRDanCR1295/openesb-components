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
 * @(#)NonEmptyScEncrypt.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp;

import java.io.BufferedReader;
import java.io.InputStreamReader;


/**
 * <p>
 * Performs encryption / decription using a single key
 * </p>
 *
 * <p></p>
 *
 * @author Robert Frank-Thompson (ported to Java by Andrea Joy Spilholtz)
 */
public class NonEmptyScEncrypt {
    /* ********************************************************* */
    /* CONSTANTS                                                 */
    /* ********************************************************* */
    
    // public final static String RCS_ID= StcCorp.COPYRIGHT +
    //  "";
    
    /* ********************************************************* */
    /* PRIVATE CONSTANTS                                         */
    /* ********************************************************* */
    
    /** keys */
    private static final char SC_M_CRYPT_CHAIN_INIT = (char) 0x72;
    
    /** keys */
    private static final short[] SC_V_CRYPT_PIN = {
        3, 5, 15, 6, 12, 9, 2, 0, 13, 10, 7, 4, 1, 14, 11, 8
    };
    
    /** keys */
    private static final short[] SC_V_CRYPT_POU = {
        7, 12, 6, 0, 11, 1, 3, 10, 15, 5, 9, 14, 4, 8, 13, 2
    };
    
    /** keys */
    private static final char[] SC_V_CRYPT_DFT_KEY = {
        (char) 0x83, (char) 0x3A, (char) 0xFC, (char) 0xE9, (char) 0x47,
        (char) 0x95, (char) 0x21, (char) 0x6D, (char) 0xBE, (char) 0x7F,
        (char) 0xA2, (char) 0x54, (char) 0xC6, (char) 0xDB, (char) 0x18,
        (char) 0x00
    };
    
    /* ********************************************************* */
    /* CONSTRUCTORS                                              */
    /* ********************************************************* */
    /* ********************************************************* */
    /* UNIT TEST                                                 */
    /* ********************************************************* */
    
    /**
     * Main driver.
     *
     * @param args arguments.
     *
     * @throws Exception on error.
     */
    public static void main(String[] args) throws Exception {
        BufferedReader in = new BufferedReader(new InputStreamReader(
                System.in));
        System.out.print("Password:  ");
        
        String password = in.readLine();
        
        System.out.print("login:  ");
        
        String Username = in.readLine();
        
        if (password.length() > 0) {
            System.out.print("> ");
//            data = in.readLine();
            
            
            // System.out.println("   (" + data.length() + ")");
            
            String newPass = NonEmptyScEncrypt.encrypt(Username, password);
            System.out.println(" EN : " + newPass);
            
            String password1 = NonEmptyScEncrypt.decrypt(Username, newPass);
            System.out.println(" DE:  " + password1);
//            }
        }
    }
    
    /* ********************************************************* */
    /* PUBLIC STATIC METHODS                                     */
    /* ********************************************************* */
    
    /**
     * Decrypt.
     *
     * @param key key.
     * @param data data
     *
     * @return decrypted string.
     *
     * @throws Exception on error.
     */
    public static String decrypt(String key, String data)
    throws Exception {
        ScSCrypt crypt;
        char[] code = new char[(data.length() / 2)];
        char[] out;
        int codePtr;
        int dataPtr;
        char abyte;
        char lbyte;
        char rbyte;
        int length;
        int count;
        int num;
        
        crypt = new ScSCrypt(key);
        dataPtr = 0;
        
        char[] lenTmp = new char[1];
        lenTmp[0] = data.charAt(dataPtr);
        
        char[] ctmp = new char[2];
        
        /* Convert hex string data to integers */
        for (count = 0; count < (data.length() / 2); count++) {
            ctmp[0] = data.charAt(dataPtr);
            ctmp[1] = data.charAt(dataPtr + 1);
            num = Integer.parseInt(new String(ctmp), 16);
            code[count] = (char) num;
            dataPtr += 2;
        }
        
        /*  Prepare to decrypt data.                                */
        dataPtr = 0;
        codePtr = 0;
        
        /*  Pop data length.                                        */
        length = (int) code[codePtr];
        codePtr++;
        
        /*  Decrypt the data.                                       */
        out = new char[length];
        
        for (count = 0; count < length; count++) {
            /*  Unchain */
            abyte = code[codePtr];
            
            // we need to keep this to 8 bits
            crypt.chain = (char) ((crypt.chain << 2) & 0x00FF);
            abyte ^= crypt.chain;
            crypt.chain = code[codePtr];
            
            /*  Split   */
            lbyte = (char) (abyte >> 4);
            rbyte = (char) (abyte & 0x0F);
            
            /*  Xor     */
            lbyte ^= crypt.keySpace[crypt.currKey];
            crypt.currKey++;
            rbyte ^= crypt.keySpace[crypt.currKey];
            crypt.currKey++;
            
            /*  Permute */
            lbyte = (char) SC_V_CRYPT_POU[(int) lbyte];
            rbyte = (char) SC_V_CRYPT_POU[(int) rbyte];
            
            /*  Join    */
            out[count] = (char) (0x00FF & ((lbyte << 4) | rbyte));
            
            codePtr++;
            dataPtr++;
            
            if (crypt.getCurrKey() == crypt.getEndSpace()) {
                crypt.extend();
            }
        }
        
        String decrypt = new String(out);
        
        return decrypt;
    }
    
    /**
     * Encrypt.
     *
     * @param key key
     * @param data data
     *
     * @return encrypted string.
     */
    public static String encrypt(String key, String data) {
        ScSCrypt crypt;
        
        // need 2 chars for the length
        char[] code = new char[(data.length() * 2) + 2];
        int codePtr;
        int dataPtr;
        char lbyte;
        char rbyte;
        int length;
        int count;
        
        crypt = new ScSCrypt(key);
        length = data.length();
        
        /*  Prepare to encrypt data.                                */
        dataPtr = 0;
        codePtr = 0;
        
        /*  Push input data length on output.                       */
        code[codePtr] = (char) length;
        codePtr++;
        
        /*  Increment length to account for pushed value.           */
        length++;
        
        /*  Encrypt the data.                                       */
        for (int i = 0; i < data.length(); i++) {
            /*  Split   */
            lbyte = (char) (data.charAt(dataPtr) >> 4);
            rbyte = (char) (data.charAt(dataPtr) & 0x0F);
            
            /*  Permute */
            lbyte = (char) SC_V_CRYPT_PIN[lbyte];
            rbyte = (char) SC_V_CRYPT_PIN[rbyte];
            
            /*  Xor     */
            lbyte ^= crypt.keySpace[crypt.currKey];
            crypt.currKey++;
            rbyte ^= crypt.keySpace[crypt.currKey];
            crypt.currKey++;
            
            /*  Join    */
            code[codePtr] = (char) (0x00FF & ((lbyte << 4) | rbyte));
            
            /*  Chain  */
            crypt.chain = (char) ((crypt.chain << 2) & 0x00FF);
            code[codePtr] ^= crypt.chain;
            crypt.chain = code[codePtr];
            
            codePtr++;
            dataPtr++;
            
            if (crypt.getCurrKey() == crypt.getEndSpace()) {
                crypt.extend();
            }
        }
        
        /*  Convert integers to a hex string.                       */
        String out = "";
        dataPtr = 0;
        
        for (count = 0; count < length; count++) {
            String digits = Integer.toHexString(code[count]).toUpperCase();
            
            if (digits.length() == 1) {
                digits = "0" + digits;
            }
            
            out += digits;
        }
        
        return out;
    }
    
    /**
     * INNER CLASS
     */
    public static class ScSCrypt {
        /** key space */
        private char[] keySpace;
        
        /** end space. */
        private int endSpace;
        
        /** current key */
        private int currKey;
        
        /** chain */
        private char chain;
        
        /**
         * Constructor.
         *
         * @param key key.
         */
        public ScSCrypt(String key) {
            String keyPtr;
            
            /*  Force a valid key.                                          */
            if ((key == null) || (key.length() == 0)) {
                keyPtr = new String(SC_V_CRYPT_DFT_KEY);
            } else {
                keyPtr = key;
            }
            
            keySpace = new char[(keyPtr.length() * 2)];
            
            /*  Prepare KeySpace.                                           */
            currKey = 0;
            
            int lookupIndex;
            
            for (int i = 0; i < keyPtr.length(); i++) {
                /*  Left side.                                              */
                lookupIndex = keyPtr.charAt(i) >> 4;
                keySpace[currKey] = (char) SC_V_CRYPT_PIN[lookupIndex];
                currKey++;
                
                /*  Right side.                                             */
                lookupIndex = keyPtr.charAt(i) & 0x0F;
                keySpace[currKey] = (char) SC_V_CRYPT_PIN[lookupIndex];
                currKey++;
            }
            
            endSpace = currKey;
            currKey = 0;
            chain = SC_M_CRYPT_CHAIN_INIT;
        }
        
        /**
         * Gets chain.
         *
         * @return chain.
         */
        char getChain() {
            return chain;
        }
        
        /**
         * Gets end space.
         *
         * @return end space
         */
        int getEndSpace() {
            return endSpace;
        }
        
        /**
         * Gets current key.
         *
         * @return current key.
         */
        int getCurrKey() {
            return currKey;
        }
        
        /**
         * Gets key space.
         *
         * @return key space array.
         */
        char[] getKeySpace() {
            return keySpace;
        }
        
        /**
         * Repermute the key space and resets the current key
         */
        void extend() {
            /* Repermute the key space.          */
            for (int i = 0; i < keySpace.length; i++) {
                keySpace[i] = (char) SC_V_CRYPT_PIN[keySpace[i]];
            }
            
            /* Reset the current key */
            currKey = 0;
        }
    }
}
