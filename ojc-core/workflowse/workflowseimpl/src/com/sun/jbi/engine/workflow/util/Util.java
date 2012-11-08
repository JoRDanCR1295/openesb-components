/* 
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License. 
 * 
 * You can obtain a copy of the license at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License. 
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * If applicable add the following below this CDDL HEADER, 
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: Portions Copyright 
 * [year] [name of copyright owner] 
 */ 
 /* 
  * $Id: Util.java,v 1.6 2010/02/15 19:24:14 fkieviet Exp $
  * 
  * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.workflow.util;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.stream.StreamSource;

import org.apache.xmlbeans.GDuration;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.workflow.model.Task;

public class Util {
    
    private static final String  WLMUSER = "wlmuser";

    public static Properties loadProps(Class clazz, String propertyFile) {
        Properties mProps = new Properties();
        URL propUrl = clazz.getResource(propertyFile);
        InputStream inputStream = null;
        try {
            inputStream = clazz.getResourceAsStream(propertyFile);
            mProps.load(inputStream);
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            throw new RuntimeException(I18n.loc(
                    "WLM-6069: File {0} not found", propUrl.toString()), e);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            throw new RuntimeException(I18n.loc(
                    "WLM-6070: IO Exception : {0} happened", propUrl.toString()), e);
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
        return mProps;

    }    
    /**
     * Convert {nameSpaceURI}localName to QName
     * @param qNameString
     * @return QName
     */    
    public static QName parseQNameString (String qNameString) {
        QName qname;
        int index = -1;
        index = qNameString.indexOf("{");
        if (index == -1) {
            qname = new QName (qNameString);
        } else {
            int lastIndex = qNameString.indexOf("}");
            String uri = qNameString.substring(index+1, lastIndex);
            String local = qNameString.substring(lastIndex+1);
            qname = new QName (uri, local);
        }
        return qname;
        
    }
    
    public static Date getDate(GDuration duration) {
        Calendar date = Calendar.getInstance();
        
        int year = duration.getYear();
        int month = duration.getMonth();
        int dayOfMonth = duration.getDay();
        int hour = duration.getHour();
        int minute = duration.getMinute();
        int second = duration.getSecond();
        BigDecimal fraction = duration.getFraction();
        
        date.add(Calendar.YEAR, year);
        date.add(Calendar.MONTH, month);
        date.add(Calendar.DAY_OF_MONTH, dayOfMonth);
        date.add(Calendar.HOUR_OF_DAY, hour);
        date.add(Calendar.MINUTE, minute);
        date.add(Calendar.SECOND, second);
        date.add(Calendar.MILLISECOND, fraction.intValue());
        
        return date.getTime();
    }
    public static Element wrapElement(Element root, String name, String namespace) throws Exception {
        // TODO Auto-generated method stub
        Element newRoot = root.getOwnerDocument().createElementNS(namespace, name);
        newRoot.appendChild(root);
        return newRoot;
    }    
    
    public static boolean isEmpty (String str) {
    	return str == null || str.trim().length() == 0;
    }
    
    public static Element getOutputInstance (Task taskMeta, EngineContext context) {
        WorkflowMapEntry entry = context.getWorkflowMapEntryTable().findWorkflowEntry(taskMeta);
        if (entry != null) {
            return entry.getOutputXformInstance();
        }
        return null;        
    	
    }
    
    public static String getStringBuffer(InputStream ss) throws Exception {
        DataInputStream dis = null;
        StringBuffer buff = new StringBuffer();
        try {
            dis = new DataInputStream(ss);

            while (dis.available() != 0) {
                buff.append((char) dis.readByte());
            }
            return buff.toString();
        } finally {
            if (dis != null)
                dis.close();
        }
    }
    
    public static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }
    
        // The directory is now empty so delete it
        return dir.delete();
    }
    public static Date getInitialDate() {
        // TODO Auto-generated method stub
       Calendar cal = Calendar.getInstance();
       cal.set(3000, 1, 1);
       return cal.getTime();
    }
    
    public static boolean isInitialDat (Calendar date) {
        if (date.get(Calendar.YEAR) == 3000) 
            return true;
        return false;
    }
    
    public static Element makeJBIReply(RuntimeTask task) throws Exception {
        Map<String, Element> partsMap = new HashMap<String, Element>();
        Task taskModel = task.getTaskMeta();
        Operation opt = taskModel.getWSDLOperation();

        Map parts = opt.getOutput().getMessage().getParts();
        String partName = (String) parts.keySet().iterator().next();
        Part part = (Part) parts.get(partName);
        QName elName = part.getElementName();
        Element wrappedEl = task.getOutput().getOutput();
        Element replyEl = wrappedEl;
        if (wrappedEl != null) {
            if (elName != null) {
                replyEl = wrappedEl.getOwnerDocument().createElementNS(
                        elName.getNamespaceURI(), elName.getLocalPart());
                NodeList nodeList = wrappedEl.getChildNodes();
                if (nodeList != null) {
                    List<Node> children = new ArrayList<Node>();
                    for (int i = 0; i < nodeList.getLength(); i++) {
                        children.add(nodeList.item(i));
                    }
                    for (int i = 0; i < children.size(); i++) {
                        replyEl.appendChild(children.get(i));
                    }

                }

            } else {
                replyEl = wrappedEl;
            }
        }
        partsMap.put(partName, replyEl);

        return JBIMessageUtil.makeJBIMessage(partsMap, opt);
    }    
    
    public  static Element makeTimeoutFault () throws Exception 
    {

            //use hardcoded system fault format for task timeout
            // This is ok to hardcode since we have only one system fault as of today
            String sysTimeoutFault = "<SystemFault version=\"1.0\" xmlns=\"http://java.sun.com/xml/ns/jbi/systemfaults\" >" +
                                     "<code>receiver</code>" +
                                     "<subcode>Timed Out</subcode>" +
                                     "<reason>This task is not completed within specified time and timed out </reason>" +
                                     "</SystemFault>";
            
            InputSource in = new InputSource (new StringReader (sysTimeoutFault));
            
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance ();
            factory.setNamespaceAware (true);
            Document doc = factory.newDocumentBuilder ().parse (in);
            
            
            Element timeoutEl = doc.getDocumentElement ();
            return timeoutEl;
            /*Map<String, Element> partsMap = new HashMap<String, Element>();
            partsMap.put ("fault", timeoutEl);
            return JBIMessageUtil.makeJBIMessage (partsMap, this.mRTaskTimer.getTask ().getTaskMeta ().getWSDLOperation ());
            */
        }
    
     public static String makeFromAddress () throws Exception
     {
         String hostName = InetAddress.getLocalHost().getCanonicalHostName();
         return WLMUSER + "@" + hostName + ".com";
     }
        
    
}
