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
 * @(#)QueryPlanImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.ExternalTablePollingStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.OperatorFactoryImpl;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ReplayStream;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.io.ByteArrayInputStream;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.XmlUtil;

import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.Collections;

/*
 * QueryPlanFactoryImpl.java
 *
 * Created on July 26, 2005, 4:07 PM
 *
 * @author Bing Lu
 */
import java.util.HashSet;
import java.util.logging.Level;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Node;
class QueryPlanImpl implements QueryPlan {
    private static final Messages mMessages = Messages.getMessages(QueryPlanImpl.class);
    
    private transient OperatorFactory mOperatorFactory;
                
    private String mId;
    private String mInstanceId;
    private String mName;
    private Properties mConfigProp;
    private final List<Operator> mOperatorList = new ArrayList<Operator>();
    private final Map<String, Operator> mOperatorMap = new HashMap<String, Operator>();
    private final Map<String, Schema> mSchemaMap = new HashMap<String, Schema>();
    private final List<Notifier> mNotifierList = new ArrayList<Notifier>();
    private final List<InvokeStream> mInvokeStreamList = new ArrayList<InvokeStream>();
    private final List<String> mDependencyIdList = new ArrayList<String>();
    
    public void addOperator(Operator op) {
        if (!hasOperator(op.getId())) {
            mOperatorList.add(op);
            mOperatorMap.put(op.getId(), op);
            if (op instanceof Notifier) {
                mNotifierList.add((Notifier)op);
            }
            if (op instanceof InvokeStream) {
                mInvokeStreamList.add((InvokeStream)op);
            }
        }
    }
    
    public void removeOperator(Operator operator) {
       if(operator != null && hasOperator(operator.getId())) {
           mOperatorList.remove(operator);
           mOperatorMap.remove(operator.getId());
       }
        
    }
    
    // public Operator getOperator(String opId) {
        // return mOperatorMap.get(opId);
    // }
    // 
    private boolean hasOperator(String opId) {
        return mOperatorMap.containsKey(opId);
    }
    
    // <component name="Plan" title="Plan" type="/EMS/Model/Plan">
    //     <property name="version" value="5.0"/>
    //     <component name="Metadata" title="Metadata" type="/EMS/Model/Plan|Metadata">
    //        ..... ignore ....
    //     </component>
    //     <component name="Schemas" title="Schemas" type="/EMS/Model/Plan|Schemas">
    //           <component name="NV" title="NV" type="/EMS/Metadata/Schema">
    //               <component name="Name" title="Name" type="/EMS/Metadata/ColumnMetadata">
    //                   <property name="name" value="Name"/>
    //                   <property name="type" value="VARCHAR"/>
    //                   <property name="size" value="20"/>
    //                   <property name="scale" value=""/>
    //               </component>
    //               .... more columns ....
    //           </component>
    //           .... more schemas ....
    //       </component>
    //       <component name="Operators" title="Operators" type="/EMS/Model/Plan|Operators">
    //           <component name="o0" title="o0" type="/EMS/Input/StreamInput">
    //               <property name="x" value="60"/>
    //               .... more properties
    //           </component>
    //           .... more operators ....
    //       </component>
    //       <component name="Links" title="Links" type="/EMS/Model/Plan|Links">
    //           .... ignore ...
    //       </component>
    // </component>
    private void loadAll(String content) {
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setValidating(false);
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(new ByteArrayInputStream(content.getBytes("UTF-8")));
            if (doc == null) {
                return;
            }
            Element root = doc.getDocumentElement();
    
            // loop through children nodes if any exist
            if (!root.hasChildNodes()) {
                return;
            }
            NodeList children = root.getChildNodes();
            // load schemas
            for (int i = 0, I = children.getLength(); i < I; i++) {
                Node node = children.item(i);
    
                if ((node == null) || 
                    (node.ELEMENT_NODE != node.getNodeType()) ||
                    (!node.getLocalName().equalsIgnoreCase("component"))) 
                {
                    continue;
                }
                NamedNodeMap attribs = node.getAttributes();
                String sName = attribs.getNamedItem("name").getNodeValue();
                if (sName.equals("Schemas")) {
                    loadSchemas((Element)node);
                } 
            }
            // load operators
            for (int i = 0, I = children.getLength(); i < I; i++) {
                Node node = children.item(i);
    
                if ((node == null) || 
                    (node.ELEMENT_NODE != node.getNodeType()) ||
                    (!node.getLocalName().equalsIgnoreCase("component"))) 
                {
                    continue;
                }
                NamedNodeMap attribs = node.getAttributes();
                String sName = attribs.getNamedItem("name").getNodeValue();
                if (sName.equals("Operators")) {
                    loadOperators((Element)node);
                }
            }
            // load dependency
            HashSet<String> dependency = new HashSet<String>();
            for (Operator op : mOperatorList) {
                for (Operator inputOp : op.getInputOperatorList()) {
                    if (inputOp.getPlan() != this) {
                        dependency.add(inputOp.getPlan().getId());
                    }
                }
            }
            mDependencyIdList.addAll(dependency);
            Collections.sort(mDependencyIdList);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "QueryPlanImpl.Fail_to_load_event_processor", mInstanceId, e);
        }
    }
    
    //     <component name="Schemas" title="Schemas" type="/EMS/Model/Plan|Schemas">
    //           <component name="NV" title="NV" type="/EMS/Metadata/Schema">
    //               <component name="Name" title="Name" type="/EMS/Metadata/ColumnMetadata">
    //                   <property name="name" value="Name"/>
    //                   <property name="type" value="VARCHAR"/>
    //                   <property name="size" value="20"/>
    //                   <property name="scale" value=""/>
    //                   <property name="comment" value=""/> <!-- ignored for runtime -->
    //               </component>
    //               .... more columns ....
    //           </component>
    //           .... more schemas ....
    //       </component>
    private void loadSchemas(Element root) {
        NodeList schemaNodeList = root.getChildNodes();
        for (int i = 0, I = schemaNodeList.getLength(); i < I; i++) {
            Node schemaNode = schemaNodeList.item(i);
            if ((schemaNode == null) || 
                (schemaNode.getNodeType() != Node.ELEMENT_NODE) ||
                (!schemaNode.getLocalName().equalsIgnoreCase("component"))) 
            {
                continue;
            }
            NamedNodeMap schemaAttrbs = schemaNode.getAttributes();
            String schemaName = schemaAttrbs.getNamedItem("name").getNodeValue();
            NodeList columnNodeList = schemaNode.getChildNodes();
            List<String> columnMetadataStrList = new ArrayList<String>(); 
            for (int j = 0, J = columnNodeList.getLength(); j < J; j++) {
                Node columnNode = columnNodeList.item(j);
                if ((columnNode == null) || 
                    (columnNode.getNodeType() != Node.ELEMENT_NODE) ||
                    (!columnNode.getLocalName().equalsIgnoreCase("component"))) 
                {
                    continue;
                }
                String columnName = "";
                String columnType = "";
                String columnSize = "";
                String columnScale = "";
                NodeList propNodeList = columnNode.getChildNodes();
                for (int k = 0, K = propNodeList.getLength(); k < K; k++) {
                    Node propNode = propNodeList.item(k);
                    if ((propNode == null) || 
                        (propNode.getNodeType() != Node.ELEMENT_NODE) ||
                        (!propNode.getLocalName().equalsIgnoreCase("property")))
                    {
                        continue;
                    }
                    NamedNodeMap propAttrbs = propNode.getAttributes();
                    String propName = propAttrbs.getNamedItem("name").getNodeValue();
                    if (propName.equalsIgnoreCase("name")) {
                        columnName = propAttrbs.getNamedItem("value").getNodeValue();
                    } else if (propName.equalsIgnoreCase("type")) {
                        columnType = propAttrbs.getNamedItem("value").getNodeValue();
                    } if (propName.equalsIgnoreCase("size")) {
                        columnSize = propAttrbs.getNamedItem("value").getNodeValue();
                    } if (propName.equalsIgnoreCase("scale")) {
                        columnScale = propAttrbs.getNamedItem("value").getNodeValue();
                    } 
                }
                columnMetadataStrList.add(columnName);
                columnMetadataStrList.add(columnType);
                columnMetadataStrList.add(columnSize);
                columnMetadataStrList.add(columnScale);
            }
            mSchemaMap.put(schemaName, new Schema(schemaName, columnMetadataStrList));
        }
    }

    //       <component name="Operators" title="Operators" type="/EMS/Model/Plan|Operators">
    //           <component name="o0" title="o0" type="/EMS/Input/StreamInput">
    //               <property name="x" value="60"/>
    //               .... more properties
    //           </component>
    //           .... more operators ....
    //       </component>
    private void loadOperators(Element root) {
        NodeList opNodeList = root.getChildNodes();
        List<OperatorSpec> opSpecList = new ArrayList<OperatorSpec>();
        for (int i = 0, I = opNodeList.getLength(); i < I; i++) {
            Node opNode = opNodeList.item(i);
            if ((opNode == null) || 
                (opNode.getNodeType() != Node.ELEMENT_NODE) ||
                (!opNode.getLocalName().equalsIgnoreCase("component"))) 
            {
                continue;
            }
            NamedNodeMap opAttrbs = opNode.getAttributes();
            String opName = opAttrbs.getNamedItem("name").getNodeValue();
            String opType = opAttrbs.getNamedItem("type").getNodeValue();
            int idx = opType.lastIndexOf('/');
            opType = opType.substring(idx + 1);
            NodeList propNodeList = opNode.getChildNodes();
            Map<String, Object> opProp = new HashMap<String, Object>(); 
            for (int j = 0, J = propNodeList.getLength(); j < J; j++) {
                Node propNode = propNodeList.item(j);
                if ((propNode == null) || 
                    (propNode.getNodeType() != Node.ELEMENT_NODE) ||
                    (!propNode.getLocalName().equalsIgnoreCase("property")))
                {
                    continue;
                }
                NamedNodeMap propAttrbs = propNode.getAttributes();
                String propName = propAttrbs.getNamedItem("name").getNodeValue();
                String propValue = propAttrbs.getNamedItem("value").getNodeValue();
                opProp.put(propName, propValue);
            }
            opProp.put(PROP_CONFIG_PROPERTIES, mConfigProp);
            opProp.put(PROP_QUERY_PLAN, this);
            opSpecList.add(new OperatorSpec(opType, opProp));
        }
        
        // Sort operators in the increasing order of topological score so that
        // an operator is always loaded after its input operators.
        Collections.sort(opSpecList);
        
        for (OperatorSpec opSpec : opSpecList) {
            Operator operator = mOperatorFactory.createOperator(opSpec);
            addOperator(operator);
        }
    }
    
    
    public QueryPlanImpl(Map prop) {
        mConfigProp = (Properties)prop.get(PROP_CONFIG_PROPERTIES);
        mOperatorFactory = new OperatorFactoryImpl();
        mId = (String)prop.get(PROP_PLAN_ID);
        mInstanceId = (String)prop.get(PROP_INSTANCE_ID);
        mName = (String)prop.get(PROP_PLAN_NAME);
        String content = (String)prop.get(PROP_PLAN_CONTENT);
        loadAll(content);
    }
        
    
    public String getId() {
        return mId;
    }

    public String getInstanceId() {
        return mInstanceId;
    }
    
    public String getName() {
        return mName;
    }
    
    public List<Operator> getOperatorList() {
        return new ArrayList<Operator>(mOperatorList);
    }
    
    public List<InvokeStream> getInvokeStreamOperatorList() {
        return new ArrayList<InvokeStream>(mInvokeStreamList);
    }
    
    public List<Notifier> getNotifierList() {
        return new ArrayList<Notifier>(mNotifierList);
    }

    public List<ExternalTablePollingStream> getExternalTablePollingStreamList() {
        List<ExternalTablePollingStream> externalTablePollingOpList = new ArrayList<ExternalTablePollingStream>();
        
        List<Operator> allOperators = getOperatorList();
        Iterator<Operator> it = allOperators.iterator();
        while(it.hasNext()) {
            Operator op = it.next();
            if(op instanceof ExternalTablePollingStream) {
                externalTablePollingOpList.add((ExternalTablePollingStream) op);
            }
        }
        
        return externalTablePollingOpList;
    }
    
    public List<ReplayStream> getReplayStreamList() {
        List<ReplayStream> replayStreamOpList = new ArrayList<ReplayStream>();
        
        List<Operator> allOperators = getOperatorList();
        Iterator<Operator> it = allOperators.iterator();
        while(it.hasNext()) {
            Operator op = it.next();
            if(op instanceof ReplayStream) {
                replayStreamOpList.add((ReplayStream) op);
            }
        }
        
        return replayStreamOpList;
    }
    
    
    public Operator getOperatorByName(String name) {
        for ( Operator op : mOperatorList) {
            if (op.getName().equalsIgnoreCase(name)) {
                return op;
            }
        }
        return null;
    }
    
    public Operator getOperatorByOperation(String operation) {
        for (Operator op : mOperatorList) {
            if (op.getOperation().equalsIgnoreCase(operation)) {
                return op;
            }
        }
        return null;
    }
    
    public Operator getOperatorById(String id) {
        return mOperatorMap.get(id);
    }

    public Schema getSchema(String id) {
        return mSchemaMap.get(id);
    }
    
    public boolean hasSchema(String id) {
        return mSchemaMap.containsKey(id);
    }
    
    public void deploy(Connection con) throws Exception {
        try {
            updateLastActivition(con, mId);
            String schemaName = mConfigProp.getProperty(PROP_DB_SCHEMA);
            int status = Util.createProcessingStateTable(con, schemaName, mId);
            if (status == TS_TABLE_EXIST) {
                // This plan is already deployed. 
                return;
            }
            
            HashMap<String, Object> initPs = new HashMap<String, Object>();
            Properties initPlanPs = new Properties();
            initPs.put(PS_PLAN_STATE, initPlanPs);
            initPs.put(PS_OPERATOR_STATE, new HashMap());
            initPlanPs.setProperty(PS_PREV_TIMESTAMP_TO_PROCESS, "0");
            initPlanPs.setProperty(PS_PREV_TIMESTAMP_TO_CHECK, "0");
            Util.initProcessingState(con, mId, initPs);

            Util.createTableUsageTable(con, schemaName, mId);
            
            List<Operator> opList = getOperatorList();
            Collections.sort(opList, OperatorComparator.getInstance());
            for (Operator op : opList) {
                op.deploy(con);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "QueryPlanImpl.Fail_to_deploy_event_processor", mInstanceId, e);
            throw e;
        } 
    }
    
    public void undeploy(Connection con) throws Exception {
        try {
            List<Operator> opList = getOperatorList();
            // undeploy is done in the reverse order of deploy
            Collections.sort(opList, Collections.reverseOrder(OperatorComparator.getInstance()));
            for (Operator op : opList) {
                op.undeploy(con);
            }
            
            Util.dropProcessingStateTable(con, mId);
            Util.dropTableUsageTable(con, mId);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "QueryPlanImpl.Fail_to_undeploy_event_processor", mInstanceId, e);
            throw e;
        }
    }

    public void deployExternalResource() throws Exception {
        try {
            List<Operator> opList = getOperatorList();
            Collections.sort(opList, OperatorComparator.getInstance());
            for (Operator op : opList) {
                op.deployExternalResource();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "QueryPlanImpl.Fail_to_deploy_external_resource_event_processor", mInstanceId, e);
            throw e;
        }
        
    }
    
    public void undeployExternalResource() throws Exception {
        try {
            List<Operator> opList = getOperatorList();
            // undeploy is done in the reverse order of deploy
            Collections.sort(opList, Collections.reverseOrder(OperatorComparator.getInstance()));
            for (Operator op : opList) {
                op.undeployExternalResource();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "QueryPlanImpl.Fail_to_undeploy_external_resource_event_processor", mInstanceId, e);
            throw e;
        }
        
    }
    
    
    private static final String UPDATE_LAST_ACTIVATION = "UPDATE " + TABLE_EMS_PLAN + " SET " + COL_LAST_ACTIVATION + " = ? WHERE " + COL_ID + " = ?";
    public static void updateLastActivition(Connection con, String planId) {
        PreparedStatement pstmt = null;
        try {
            pstmt = con.prepareStatement(UPDATE_LAST_ACTIVATION);
            pstmt.setTimestamp(1, new Timestamp(System.currentTimeMillis()));
            pstmt.setString(2, planId);
            pstmt.executeUpdate();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "QueryPlanImpl.Fail_to_update_event_processor_s_last_activation_time", planId, e);
        } finally {
            Util.close(pstmt);
        }
    }
    
    public Properties getConfigProperties() {
        return this.mConfigProp;
    }
    
    public void changeAndPersistOperatorProperty(Connection con , Operator opr,
            String propName, String propValue) throws Exception {
        String oprIdName = opr.getId();
        String content = Util.getPlanContent(con, getInstanceId());
        
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setValidating(false);
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.parse(new ByteArrayInputStream(content.getBytes("UTF-8")));
        if (doc == null) {
            return;
        }
        Element root = doc.getDocumentElement();
        String xPathString = "/pre:component/pre:component[@name='Operators']/pre:component[@name='"+ oprIdName+
                "']/pre:property[@name='"+ propName +"']/@value";
        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(new PersonalNamespaceContext());
        XPathExpression expr = xpath.compile(xPathString);
        Object result = expr.evaluate(doc, XPathConstants.NODE);
        Node n = (Node)result;
        n.setNodeValue(propValue);
        String nContent = XmlUtil.toXml(doc.getDocumentElement(), "UTF-8", false);
        Util.setPlanContent(con, getInstanceId(),nContent);
        
        //update plan version
        //after plan is updated in EMS_PLAN tables
        //create a new version of plan in EMS_PLAN_VERSIONS
        Util.insertNewEMSPlanVersion(con, getInstanceId(), getName(), nContent.getBytes("UTF-8"));
    }

    public List<String> getDependencyIdList() {
        return mDependencyIdList;
    }
}
class PersonalNamespaceContext implements NamespaceContext {

    public String getNamespaceURI(String prefix) {
        if (prefix == null) throw new NullPointerException("Null prefix");
        else if ("pre".equals(prefix)) return "http://jbi.com.sun/iep";
        else if ("xml".equals(prefix)) return XMLConstants.XML_NS_URI;
        return XMLConstants.NULL_NS_URI;
    }

    // This method isn't necessary for XPath processing.
    public String getPrefix(String uri) {
        throw new UnsupportedOperationException();
    }

    // This method isn't necessary for XPath processing either.
    public Iterator getPrefixes(String uri) {
        throw new UnsupportedOperationException();
    }

}
