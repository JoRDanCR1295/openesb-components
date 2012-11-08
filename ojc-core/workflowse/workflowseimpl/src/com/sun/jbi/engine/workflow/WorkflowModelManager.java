/* *************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.engine.workflow;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.model.ModelFactory;
import com.sun.jbi.workflow.model.Task;

/**
 *
 *  This class is used to obtain the Task Model from workflow file
 *  and form relation between PartnerLink+PartnerLinkType to TaskModel.
 *  This class is used by ServiceUnitManager to obtain the taskmodel
 *  @author sgenipudi
 */
public class WorkflowModelManager {
    //List of Workflow Models
    private static final Logger mLogger = Logger.getLogger(WorkflowModelManager.class.getName());
    private Map<QName, Element> mXFormMap = new HashMap<QName, Element> ();
    private Map<QName, Element> mInputXFormInstanceMap = new HashMap<QName, Element> ();
    private Map<QName, Element> mOutputXFormInstanceMap = new HashMap<QName, Element> ();  
    
    private DocumentBuilderFactory mFactory  = DocumentBuilderFactory.newInstance();
//    private static final String INPUT = "Input.xhtml";
//    private static final String OUTPUT = "Output.xhtml";
    private static final String INPUT_INSTANCE = "InputInstance.xml";
    private static final String OUTPUT_INSTANCE = "OutputInstance.xml";
    
    
    public WorkflowModelManager() {
   
    }
    
    public void loadXform (File xForm, Map<QName, WorkflowMapEntry> wlmMapEntryProvides) throws Exception {
        String name = null;
        String fileName = xForm.getName();
        if (fileName.endsWith("Xform.xhtml")) {
            name = fileName.substring(0, fileName.indexOf("Xform.xhtml"));
            FileInputStream fis = new FileInputStream(xForm);
            
            byte[] contents = new byte[fis.available()];
            fis.read(contents, 0, contents.length);
            fis.close();
            
            mFactory.setNamespaceAware(true);          

            InputSource in = new InputSource(new ByteArrayInputStream (contents));
            in.setEncoding("UTF-8");
            Document doc = mFactory.newDocumentBuilder().parse(in);
            
            Element el = doc.getDocumentElement();
            
            Set<QName> taskNames = wlmMapEntryProvides.keySet();
            for (QName qName : taskNames) {
                if (qName.getLocalPart().equals(name)) {
                    mXFormMap.put(qName, el);
                    WorkflowMapEntry entry = wlmMapEntryProvides.get(qName);
                    if (entry != null){
                        entry.setXform(el);                       
                    }
                    break;
                } 
            }
            
//            String xml = XmlUtil.toXml(el, "UTF-8", true);
//            byte[] utf8Bytes = xml.getBytes("UTF8");
//            FileOutputStream fout= new FileOutputStream("XformCallResult001.xml");
//            // Open an output stream
//            fout.write(utf8Bytes);
//            fout.flush();
//            fout.close();       
        }

    }
    
    public void loadXFormInstance(File instance, Map<QName, WorkflowMapEntry> wlmMapEntryProvides) throws Exception {
        // TODO Auto-generated method stub
        String name = null;
        Map addTo = null;  
        String fileName = instance.getName();          
        if (fileName.endsWith(INPUT_INSTANCE)){
            name = fileName.substring(0, fileName.indexOf(INPUT_INSTANCE));
            addTo = mInputXFormInstanceMap;
        } else if (fileName.endsWith(OUTPUT_INSTANCE)){
            name = fileName.substring(0, fileName.indexOf(OUTPUT_INSTANCE));
            addTo = mOutputXFormInstanceMap;
        } else {
            return;
        }
        
        FileInputStream fis = new FileInputStream(instance);
        
        byte[] contents = new byte[fis.available()];
        fis.read(contents, 0, contents.length);
        fis.close();
        
        mFactory.setNamespaceAware(true);          

        InputSource in = new InputSource(new ByteArrayInputStream (contents));
        in.setEncoding("UTF-8");
        Document doc = mFactory.newDocumentBuilder().parse(in);
        Set<QName> taskNames = wlmMapEntryProvides.keySet();
        
        for (QName qName : taskNames) {
            if (qName.getLocalPart().equals(name)) {
                Element el = doc.getDocumentElement();
                addTo.put(qName, el);
                WorkflowMapEntry entry = wlmMapEntryProvides.get(qName);
                if (entry != null){
                    if (addTo == mInputXFormInstanceMap) 
                    {
                        entry.setInputXformInstance(el); 
                    } 
                    else {
                         entry.setOutputXformInstance(el);
                     }
                
            } 
                break;
        }

        }
        
    }    
    
    public Element getXform (QName taskName) {
        return mXFormMap.get(taskName);
    }
    
        
    public Element getInputXformInstance (QName taskName) {
        return mInputXFormInstanceMap.get(taskName);
    }
    
    public Element getOutputXformInstance  (QName taskName) {
        return mOutputXFormInstanceMap.get(taskName);
    }
    
    
    public Task loadWorkFlowModel(File workFlowFile) {
        Task task = null;
        try {
            task =ModelFactory.getInstance().getTaskModel(workFlowFile.getAbsolutePath());
        }catch (Exception me) {
            mLogger.severe(I18n.loc("WLM-7000: Error creating WorkFlowModel {0}. Reason {1}", workFlowFile.getAbsolutePath(), me));
        }
        return task;
    }
    
    
     
        

    
    /**
     *  Utility class holding the relation between 
     *  PartnerLink / PartnerLinkType and The TaskModel
     */
   private class WorkFlowModel {
        // QName for partnerLink
        private QName mQNameOfPT = null;
        //QName for Partnerlink Type
        private String mOpt = null;
        //Task Model
        private Task mTask = null;
        
        WorkFlowModel(QName qNameOfPL, String opt, Task task ) {
            mQNameOfPT = qNameOfPL;
            mOpt = opt;
            mTask = task;
        }
        
        public boolean equals(Object obj) {
            if (obj != null && obj instanceof WorkFlowModel) {
                WorkFlowModel workFlowObj = (WorkFlowModel)obj;
                if (workFlowObj.getQNameForPortType()!= null && 
                    workFlowObj.getOpt() != null && 
                    workFlowObj.getQNameForPortType().equals(mQNameOfPT) &&
                    workFlowObj.getOpt().equals(mOpt)) {
                        return true;
                    }
            }
            return false;
        }
        
        public int hashCode() {
            if (mQNameOfPT != null && mOpt != null ) {
                return mQNameOfPT.hashCode()+mOpt.hashCode();
            }
            return 1;
        }

        public QName getQNameForPortType() {
            return mQNameOfPT;
        }

        public void setQNameForPortType(QName mQNameOfPL) {
            this.mQNameOfPT = mQNameOfPL;
        }

        public String getOpt() {
            return mOpt;
        }

        public void setOpt(String oPt) {
            this.mOpt = oPt;
        }

        public Task getTask() {
            return mTask;
        }
        
    }






}
