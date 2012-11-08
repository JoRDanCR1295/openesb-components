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
 * @(#)QueryPlanInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */



package com.sun.jbi.engine.iep.core.runtime.operator;

import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.Serializable;
import java.util.logging.Level;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/*
 * QueryPlanInfo.java
 * 
 * Created on Sep 14, 2007, 1:04:53 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * @author Bing Lu
 */
public class QueryPlanInfo implements Serializable {
    private static final Messages mMessages = Messages.getMessages(QueryPlanInfo.class);

    private String mInstanceId;
    private String mName;
    private String mPackage;
    private String mPlanFilePath;

    public QueryPlanInfo(String instanceId, String planFilePath) throws Exception {
        mInstanceId = instanceId;
        mPlanFilePath = planFilePath;
        File planFile = new File(mPlanFilePath);
        String planFileName = planFile.getName();
        int idx = planFileName.lastIndexOf('.');
        if (idx < 0 || !planFileName.substring(idx + 1).equals("iep")) {
            String msg = mMessages.getString("QueryPlanInfo.Event_processor_file_must_have_extension_iep");
            throw new IllegalArgumentException(msg);
        }
        mName = planFileName.substring(0, idx);
        try {
            byte[] planContent = IOUtil.getBytes(planFilePath);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setValidating(false);
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(new ByteArrayInputStream(planContent));
            if (doc == null) {
                return;
            }
            Element root = doc.getDocumentElement();
            if (root.hasAttribute("packageName")) {
                mPackage = root.getAttribute("packageName");
            } else {
                mPackage = "";
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "QueryPlanImpl.Fail_to_load_event_processor", mInstanceId, e);
        }

    }
    
    public String getInstanceId() {
        return mInstanceId;
    }
    
    public String getName() {
        return mName;
    }

    public String getPackage() {
        return mPackage;
    }

    public String getFullName() {
        if (mPackage.equals("")) {
            return mName;
        }
        return mPackage + "." + mName;
    }
    
    public String getPlanFilePath() {
        return mPlanFilePath;
    }

}
