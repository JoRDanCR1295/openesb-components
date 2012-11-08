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
 */

/*
 * @(#)$Id: MQCharacterSets.java,v 1.1 2008/12/16 21:48:12 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * MQ Character Sets.
 * 
 * @author Noel.Ang@sun.com
 */
public final class MQCharacterSets {
    private static final Map<Integer, String> cMqToJavaCharsetMap =
            new HashMap<Integer, String>();
    static {
        // These are the TOTAL set of documented character set values
        // defined in the WebSphere MQ Base Java classes API documentation.
        // If we encounter a character set value that is unaccounted for,
        // it's IBM's fault.
        //
        // The keys are taken from the MQ documentation, see MQMD.characterSet.
        // The values are character set names taken from the IANA registry.
        //    Preferred MIME names are used when available.  Otherwise
        //    their registry names are used.
        //
        // See java.nio.charset.Charset for stipulation regarding charset
        // canonical names based on preferred MIME names before registry names,
        // before alias.
        //
        // IANA charset registry here: http://www.iana.org/assignments/character-sets
        // WHere did I find the more obscure mappings (like character set 1200)?
        // MSDN Internet Explorer Developer Center documentation.
        cMqToJavaCharsetMap.put(37, "IBM037");
        cMqToJavaCharsetMap.put(819, "ISO-8859-1");
        cMqToJavaCharsetMap.put(850, "IBM850");
        cMqToJavaCharsetMap.put(1200, "UTF-16");
        cMqToJavaCharsetMap.put(1208, "UTF-8");
        // These I added because I refuse to believe these won't be encountered
        cMqToJavaCharsetMap.put(437, "IBM437");
        cMqToJavaCharsetMap.put(367, "US-ASCII");
    }
    
    private MQCharacterSets() {
    }
    
    public static String toJavaCharset(int ccsid) {
        return cMqToJavaCharsetMap.get(ccsid);
    }
    
    public static Collection<Integer> mqCharsets() {
        return Collections.unmodifiableCollection(cMqToJavaCharsetMap.keySet());
    }
    
    public static Collection<String> charsets() {
        return Collections.unmodifiableCollection(cMqToJavaCharsetMap.values());
    }
}
