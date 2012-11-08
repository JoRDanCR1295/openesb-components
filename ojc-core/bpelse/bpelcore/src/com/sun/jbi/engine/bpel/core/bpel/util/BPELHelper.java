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
 * @(#)BPELHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.util;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.rmi.server.UID;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;

import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.bpel.xml.common.model.XMLNode;


/**
 * BPELHelper class
 *
 * @author Sun Microsystems
 * @version 
 */
public class BPELHelper {
    /**
     * serial version UID
     */
    static final long serialVersionUID = 0xaa77c6a475c40b97L;
    private static String localIPAddress;

    static {
        try {
            localIPAddress = InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException unknownhostexception) {
            localIPAddress = I18n.loc("BPCOR-3057: localhost");
        }
    }
    private static Map<String, Integer> guidMap = Collections.synchronizedMap(new WeakHashMap<String, Integer> ());
    private static Integer seq = 0;
    

    /**
     * Creates a Globally Unique Identifier.
     *
     * @return String
     */
    public String getUID() {
        String uid = new UID().toString();

        //uid = "BPID--" + bpId ++; //
        return (uid);
    }

    /**
     * get GUID
     *
     * @return String GUID
     */
    public static String getGUID() {
        UID uid = new UID();
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append(localIPAddress);
        stringbuffer.append(":"); // NO I18N
        stringbuffer.append(uid.toString());
        String guid = stringbuffer.toString();

        return guid;
    }
    
    /**
     * Returns the global sequence number for this GUID
     * @param guid
     * @return
     */
    public static Integer getSeq (String guid) {
        return guidMap.get(guid);
    }

    /**
     * make key string
     * @param process runtime BPEL process
     * @return String key string
     */
    public static String makeKey(RBPELProcess process) {
        return new StringBuffer("{").append(process.getTargetNamespace())  // NO I18N
                                    .append("}").append(process.getName()) // NO I18N
                                    .toString();
    }
    
    public static String getNameInPath(String path, String separator) {
        if ((path.length() == 1) && path.equals(separator)) {
            path = "";
        } else if (path.endsWith(separator)) {
            path = path.substring(0, path.length() - 1);
        }

        int lastSlashIndex = path.lastIndexOf(separator);
        int nameStart = (lastSlashIndex >= 0) ? (lastSlashIndex + 1) : 0;

        return path.substring(nameStart);
    }

    public static String getProcessURI(ActivityUnit pc) {
        if (pc != null) {
            XMLNode act = pc.getStaticModelActivity();

            while (!(act instanceof BPELProcess)) {
                act = act.getParent();
            }

            return getProcessURI((BPELProcess) act);
        }

        return null;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param proc DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getProcessURI(BPELProcess proc) {
        return proc.getTargetNamespace() + "/" + proc.getName();
    }

    public static void addUID(String guid) {
        // TODO Auto-generated method stub
        guidMap.put(guid, ++seq);
    }
    
    public static void removeUID (String guid) {
        guidMap.remove(guid);
    }
}
