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
 * @(#)ExecAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

import java.util.ArrayList;
import java.util.List;

/**
 * The instance of this class holds a delimiter set and does delimiter matching.
 * 
 * @author Jun Xu
 */
public class Delimiters {

    private byte[][][] _delims;
    private String _delimSetDesc;
    
    public Delimiters(String delimSetDesc) {
        _delims = parseDelimSet(delimSetDesc);
        _delimSetDesc = delimSetDesc;
    }
    
    public Match match(byte[] data) {
        if (data == null) {
            throw new NullPointerException("no data");
        }
        return match(data, 0, data.length);
    }
    
    public Match match(byte[] data, final int start, final int len) {
        if (data == null) {
            throw new NullPointerException("no data");
        }
        if (start < 0 || len < 0 || start >= data.length ||
                start + len > data.length) {
            throw new ArrayIndexOutOfBoundsException(
                    "dataLen=" + data.length
                    + ", start=" + start + ", len=" + len);
        }
        final int end = start + len;
        byte[] delim = null;
        int i;
        int hash;
        dataLoop: for (i = start; i < end; i++) {
            hash = 0xff & data[i];
            if (_delims[hash] != null) {
                byte[][] delims = _delims[hash];
                final int limit = end - i;
                delimLoop: for (int j = 0; j < delims.length; j++) {
                    for (int k = 1; k < delims[j].length && k < limit; k++) {
                        if (delims[j][k] != data[i + k]) {
                            continue delimLoop;
                        }
                    }
                    delim = delims[j];
                    break dataLoop;
                }
            }
        }
        if (delim != null) {
            return new Match(i, delim);
        }
        return null;
    }
    
    private byte[][][] parseDelimSet(String desc) {
        List<byte[]> delimList = new ArrayList<byte[]>();
        StringBuilder sb = new StringBuilder();
        
        int i = 0;
        while(i < desc.length()) {
            char c = desc.charAt(i);
            if (c == '\\') {
                if (i >= desc.length() - 1) {
                    sb.append(c);
                    break;
                }
                switch (desc.charAt(i + 1)) {
                case 'n':
                    sb.append('\n');
                    i += 2;
                    break;
                case 'r':
                    sb.append('\r');
                    i += 2;
                    break;
                case 't':
                    sb.append('\t');
                    i += 2;
                    break;
                case 'b':
                    sb.append('\b');
                    i += 2;
                    break;
                case 'u':
                    if (i + 5 < desc.length()) {
                        sb.append(Character.toChars(
                                Integer.parseInt(
                                        desc.substring(i + 2, i + 6), 16)));
                        i += 6;
                    } else {
                        sb.append(desc.charAt(i + 1));
                        i += 2;
                    }
                    break;
                default:
                    sb.append(desc.charAt(i + 1));
                    i += 2;
                }
            } else if (c == ',') {
                if (sb.length() > 0) {
                    //Not considering character encoding for now
                    delimList.add(sb.toString().getBytes());
                    sb.setLength(0);
                }
                i++;
            } else {
                sb.append(c);
                i++;
            }
        }
        if (sb.length() > 0) {
            delimList.add(sb.toString().getBytes());
        }
        byte[][][] delims = new byte[256][][];
        for (byte[] delim : delimList) {
            int hash = 0xff & delim[0];
            if (delims[hash] == null) {
                byte[][] multiDelim = new byte[1][];
                multiDelim[0] = delim;
                delims[hash] = multiDelim; 
            } else {
                byte[][] multiDelim =
                    new byte[delims[hash].length + 1][];
                System.arraycopy(delims[hash], 0, multiDelim, 0,
                        delims[hash].length);
                multiDelim[delims[hash].length] = delim;
                delims[0xff & delim[0]] = multiDelim;
            }
        }
        
        return delims;
    }

    @Override
    public String toString() {
        return _delimSetDesc;
    }
    
    public static class Match {
        
        public final int _pos;
        public final byte[] _delim;
        public final int _nextStart;
        
        public Match(int pos, byte[] delim) {
            _pos = pos;
            _delim = delim;
            _nextStart = _pos + _delim.length;
        }
    }
}
