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
 * @(#)MapUtils.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.eways.util;

import java.util.Map;
import java.util.Iterator;
import java.util.HashMap;

import java.io.IOException;


public class MapUtils {

    /** 
     * default field separator string
     */
    public static final String FIELD_SEP = ",";

    /** 
     * default record separator string 
     * defaults to <code>System.getProperty("line.separator")</code>
     */
    public static final String RECORD_SEP = System.getProperty("line.separator");

    /**
     * default key
     */
    public static final String DEFAULT_KEY = "%default%";

    /**
     * map cache
     */
    private static Map mapCache = new HashMap(101);

    /**
     * hidden constructor
     */
    private MapUtils (){}

    /**
     * parse a map from a string
     *
     * If _fieldSep or _recSep appear within a field it should be escaped
     * using <code>StringUtils.escape()</code>.
     *
     * @param _string string to parse.  preferably generated by * <code>renderMap</code>.
     * @param _fieldSep field separator string
     * @param _recSep record separator string
     *
     * @return the parsed map.
     *
     * @see StringUtils#escape(String)
     */
    public static Map parseMap (String _string, String _fieldSep, String _recSep) {
        Map map = new HashMap();

        // cache lengths
        int strlen = _string.length();
        int fslen = _fieldSep.length();
        int rslen = _recSep.length();

        // break up each record
        int start = 0;
        int pos = 0;
        while(pos<strlen) {
            int fieldSepPos = 0;
            int recSepPos = 0;

            // find field separator
            while(0 == fieldSepPos) {
                pos = _string.indexOf(_fieldSep, pos);

                // check for escape
                if(pos>0 && _string.charAt(pos-1) == StringUtils.DEFAULT_ESCAPE) {
                    pos+=fslen;
                } else {
                    fieldSepPos=pos;
                }
            }
            pos = fieldSepPos+fslen;

            String key = StringUtils.unescape(_string.substring(start, fieldSepPos));

            // find record separator
            start = pos;
            while(0 == recSepPos) {
                pos = _string.indexOf(_recSep, pos);

                // handle missing final delimiter
                if(-1==pos) {
                    recSepPos=strlen;
                    break;
                }

                // check for escape
                if(pos>0 && _string.charAt(pos-1) == StringUtils.DEFAULT_ESCAPE) {
                    pos+=rslen;
                } else {
                    recSepPos=pos;
                }
            }
            pos=recSepPos+rslen;
            
            String value = StringUtils.unescape(_string.substring(start, recSepPos));
            map.put(key, value);
            start = pos;
        }

        return map;
    }

    /**
     * Render a map to a string suitable for later parsing by parseMap.
     *
     * If _fieldSep or _recSep appear within a field it should be escaped.
     *
     * @param _map map to render
     * @param _fieldSep field separator string
     * @param _recSep record separator string
     *
     * @return a string that can be parsed into a map using <code>parseMap</code>.
     *
     * @see #parseMap(String,String,String)
     */
    public static String renderMap (Map _map, String _fieldSep, String _recSep) {
        StringBuffer buf = new StringBuffer();

        Iterator iter = _map.entrySet().iterator();
        while(iter.hasNext()) {
            Map.Entry entry = (Map.Entry)iter.next();
            buf.append(StringUtils.escape(entry.getKey().toString(), _fieldSep));
            buf.append(_fieldSep);
            buf.append(StringUtils.escape(entry.getValue().toString(), _recSep));
            buf.append(_recSep);
        }

        return buf.toString();
    }

    /** 
     * write a map to the specified file using default field and
     * record separators.
     *
     * @param _fileName name of file
     * @param _map map to write
     *
     * @throws IOException unable to write map to file
     *
     * @see #writeMap(String, Map, string, string)
     * @see #FIELD_SEP
     * @see #RECORD_SEP
     */
    public static void writeMap (String _fileName, Map _map) throws IOException {
        writeMap(_fileName, _map, MapUtils.FIELD_SEP, MapUtils.RECORD_SEP);
    }

     /**
      * write a map to the specified file and parse
      * using specified separators
      *
      * @param _fileName name of file
      * @param _map map to write
      * @param _fieldSep field separator string
      * @param _recSep record separator string
      *
      * @throws IOException  unable to write map to file
      */
    public static void writeMap (String _fileName, Map _map, String _fieldSep, String _recSep) throws IOException {
        FileUtils.write(_fileName, renderMap(_map, _fieldSep, _recSep));
    }

    /**
     * read a map from the specified file using default field and
     * record separators.
     *
     * @param _fileName name of file
     *
     * @return the map
     *
     * @throws IOException unable to read map from file
     *
     * @see #readMap(String,String,String)
     * @see #FIELD_SEP
     * @see #RECORD_SEP
     */
     public static Map readMap (String _fileName) throws IOException {
        return readMap(_fileName, MapUtils.FIELD_SEP, MapUtils.RECORD_SEP);
     }

     /**
      * read a map from the specified file and parse
      * using specified separators
      *
      * @param _fileName name of file
      * @param _fieldSep field separator string
      * @param _recSep record separator string
      *
      * @return the map
      *
      * @throws IOException unable to read map from file
      *
      * @see FileUtils#readString(String)
      * @see #parseMap(String,String,String)
      */
    public static Map readMap (String _fileName, String _fieldSep, String _recSep) throws IOException {
        return parseMap(FileUtils.readString(_fileName), _fieldSep, _recSep);
    }


    /**
     * look up a string in a map
     *
     * if the value is not found, a default value is returned
     * The default value has either an empty string or <code>DEFAULT_KEY</code>
     * as its key.
     *
     * @param _map map to use
     * @param _key key to lookup
     *
     * @return string containing value (null if not in map)
     */
    public static String doMap (Map _map, String _key) {
        Object value = _map.get(_key);
        if(null==value) value = _map.get(DEFAULT_KEY);
        if(null==value) value = _map.get("");
        if(null==value) return null;
        return value.toString();
    }

    /**
     * Look up a string in a cached map.
     *
     * If the map is not in the cache, it is loaded from disk.  The
     * map must have been written to disk using the <code>writeMap(String)</code>
     * method (ie. it must have been written using the default delimiters).
     *
     * @param _fileName name of file containing map (also used as cache key)
     * @param _key key to lookup
     *
     * @return string containing the value (null if not in map)
     *
     * @throws IOException unable to read map from file
     *
     * @see #writeMap(String)
     */
    public static String doMap (String _fileName, String _key) throws IOException {

        Map theMap = (Map)mapCache.get(_fileName);
        if(null == theMap) {
            theMap = readMap(_fileName);
            mapCache.put(_fileName, theMap);
        }
        
        return doMap(theMap, _key);
    }

    /** 
     * test to see if file is cached for <code>doMap</code> calls.
     *
     * @param _fileName name of the file
     *
     * @return true if the file is cached
     */
    public static boolean isCached (String _fileName) {
        return null != mapCache.get(_fileName);
    }

}
