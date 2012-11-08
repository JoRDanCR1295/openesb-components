package com.sun.jbi.engine.iep.core.runtime.operator;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.OperatorFactoryImpl;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.SaveStream;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.XmlUtil;
import com.sun.jbi.engine.iep.core.share.SharedConstants;

/**
 * 
 * @author radval
 *
 */
public class OperatorHelper {

    private static final Messages mMessages = Messages.getMessages(OperatorHelper.class);
    
    private Logger mLogger = Logger.getLogger(OperatorHelper.class.getName());
    
    private static OperatorHelper mInstance;
    
    private OperatorFactory mOperatorFactory = null;
    
    
    private OperatorHelper() {
        mOperatorFactory = new OperatorFactoryImpl();
        
    }
    
    public static synchronized  OperatorHelper getDefault() {
    
        if(mInstance == null) {
            mInstance = new OperatorHelper();
        }
        
        return mInstance;
    }
    
    public synchronized SaveStream  createAndAddSaveStream(String streamInputName,
                                             QueryPlan plan,
                                             String jndiName,
                                             String tableName,
                                             String isGlobal) throws Exception {
        SaveStream saveStream = null;
        
        Operator operator = plan.getOperatorByName(streamInputName);
        if(operator != null) {
            String outputType = operator.getOutputType();
            if(SharedConstants.IO_TYPE_STREAM.equals(outputType)) {
                //create SaveStream Operator.
                saveStream = createSaveStream(plan, operator, jndiName, tableName, isGlobal);
                if(saveStream != null){
                    //add save stream to plan
                    plan.addOperator(saveStream);
                }
            } else {
                //failed to add save stream on an operator
                //this means the operator we are trying
                //to add save stream does not outputs a stream
                String msg = mMessages.getString("OperatorHelper.Failed_to_add_save_stream", new Object[]{streamInputName, plan.getName()});
                
                throw new Exception(msg);
            }
            
        } else {
            //we are not able to find operator to which we can add SaveStream
            //operator
            String msg = mMessages.getString("OperatorHelper.Failed_to_find_operator_in_plan", new Object[]{streamInputName, plan.getName()});
            throw new Exception(msg);
            
            
        }
        return saveStream;
    }
    
    public synchronized boolean removeSaveStream(String saveStreamName,
                                    QueryPlan plan) {
        
        boolean result = false;
        
        if(saveStreamName != null && plan != null) {
            Operator operator = plan.getOperatorByName(saveStreamName);
            if(operator != null) {
                plan.removeOperator(operator);
                result = true;
            }
        }
        
        return result;
    }
    
    private SaveStream createSaveStream(QueryPlan plan, 
                                        Operator streamOutOperator,
                                        String jndiName,
                                        String tableName,
                                        String isGlobal) {
        SaveStream s = null;
        
        
        Element element = loadOperatorTemplate("templates/saveStreamTemplate.xml");
        
        if(element != null) {
            OperatorSpec opSpec = createOperatorSpec(element, plan);
            
            if(opSpec != null) {
                Map streamInputProperties = streamOutOperator.getOperatorProperties();
                //now set some properties which are required for this operator to work properly
                Map<String, Object> prop = opSpec.getOperatorProperties();
                
                //name
                String operatorName = getNewOperatorName(streamOutOperator.getName() + "SaveStream", plan);
                prop.put(SharedConstants.PROP_NAME, operatorName);
                
                //isGlobal
                boolean isGlobalBool = true;
                if(isGlobal != null && isGlobal.equals("false")) {
                    isGlobalBool =  false; 
                }
                prop.put(SharedConstants.PROP_IS_PRESERVE_TABLE, Boolean.toString(isGlobalBool));
                        
                        
                
                //id
                String id = getNewOperatorId(plan);
                prop.put(SharedConstants.PROP_ID, id);
                
                //inputSchemaIdList
                String outputSchemaId = (String) streamInputProperties.get(SharedConstants.PROP_OUTPUT_SCHEMA_ID);
                prop.put(SharedConstants.PROP_INPUT_SCHEMA_ID_LIST, outputSchemaId);
                
                //outputSchemaId
                
                prop.put(SharedConstants.PROP_OUTPUT_SCHEMA_ID, outputSchemaId);
                
                
                //inputIdList
                prop.put(SharedConstants.PROP_INPUT_ID_LIST, streamOutOperator.getId());
                
                //databaseJndiName
                prop.put(SharedConstants.PROP_DATABASE_JNDI_NAME,  jndiName);
                
                //tableName 
                prop.put(SharedConstants.PROP_TABLE_NAME, tableName);
                
//              toposcore
                //calculate the new topoScore based on the source
                //operator toposcore where we are connecting this operator.
                int topoScore = adjustedTopoScore(streamOutOperator);
                prop.put(SharedConstants.PROP_TOPO_SCORE, topoScore);
                
                s = (SaveStream) mOperatorFactory.createOperator(opSpec);
                
                
                
            }
        }
        return s;
    }
    
    
    private Element loadOperatorTemplate(String templateLocation) {
        Element element = null;
        
        try {
            InputStream in = OperatorHelper.class.getResourceAsStream(templateLocation);
            InputSource source = new InputSource(in);
            Document document = XmlUtil.createDocument(true, source);
            if(document != null) {
                element = document.getDocumentElement();
            }
        } catch(Exception ex) {
            mMessages.log(Level.SEVERE, "OperatorHelper.Failed_to_parse_template", templateLocation, ex);
            
        }
        return element;
    }
    
    private OperatorSpec createOperatorSpec(Element element, QueryPlan plan) {
        OperatorSpec operatorSpec = null;
        
        Node opNode = element;
        if ((opNode == null) || 
            (opNode.getNodeType() != Node.ELEMENT_NODE) ||
            (!opNode.getLocalName().equalsIgnoreCase("component"))) 
        {
            return null;
        }
        
        NamedNodeMap opAttrbs = opNode.getAttributes();
//        String opName = opAttrbs.getNamedItem("name").getNodeValue();
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
        
        Properties configProp = plan.getConfigProperties();
        
        opProp.put(OperatorConstants.PROP_CONFIG_PROPERTIES, configProp);
        opProp.put(OperatorConstants.PROP_QUERY_PLAN, plan);
        
        operatorSpec = new OperatorSpec(opType, opProp); 
        
        
        return operatorSpec;
    }
    
    private String getNewOperatorName(String prefix, QueryPlan plan) {
        String name = null;
        int counter = 0;
        
        String newOperatorName = prefix + counter;
        while(isNameTaken(newOperatorName, plan)) {
            counter++;
            newOperatorName = prefix + counter;
        }
        
        name = newOperatorName;
        return name;
    }
    
    private boolean isNameTaken(String newOperatorName, QueryPlan plan) {
        boolean result = false;
        
        List<Operator> operators = plan.getOperatorList();
        Iterator<Operator> it = operators.iterator();
        while(it.hasNext()) {
            Operator operator = it.next();
            String opName = operator.getName(); 
            if(newOperatorName.equals(opName)){
                result = true;
                break;
            }
        }
        
        return result;
    }
    
    private String getNewOperatorId(QueryPlan plan) {
        String id = null;
        int counter = 0;
        String newId = "o" + counter;
        while(isIdTaken(newId, plan)) {
            counter++;
            newId = "o" + counter;
        }
        id = newId;
        return id;
    }
    
    private boolean isIdTaken(String newId, QueryPlan plan) {
        boolean result = false;
        
        List<Operator> operators = plan.getOperatorList();
        Iterator<Operator> it = operators.iterator();
        while(it.hasNext()) {
            Operator operator = it.next();
            String opId = operator.getId(); 
            if(newId.equals(opId)){
                result = true;
                break;
            }
        }
        
        return result;
    }
    
    private int adjustedTopoScore(Operator streamOutOperator) {
        int topoScore = streamOutOperator.getTopoScore();
        return topoScore++;
    }
}
