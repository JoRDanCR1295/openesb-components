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

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.workflow.model.Task;

public class WorkflowMapEntry {
    
    
    private EntryType mType;
    
    private QName mService;
    private QName mInterface;
    private String mEndpointName;
    private String mOperation;
    private List<String> mServiceUnitNames;
    private Task mTaskModel;

    private Definition mWsdl;
    private boolean mStarted = true;
    private ServiceEndpoint mServiceEndpoint;
    private String mEndpointStatusId;
    private EndpointStatus mEndpointStatus;
    private Element mXform;
    private Element mInputXformInstance;
    private Element mOutputXformInstance;
         
    private static final Logger mLogger = Logger.getLogger(WorkflowMapEntry.class.getName());
    
    /**
     * Creates a new instance of WorkFlowMap Entry
     */
    public WorkflowMapEntry(QName service, 
    						QName interfce, 
    						String endpoint, 
    						Task taskModel,
    						EntryType type) {
        mService = service;
        mInterface = interfce;
        mEndpointName = endpoint;
        mTaskModel= taskModel;
        this.mType = type;
        mServiceUnitNames = new ArrayList<String> ();
    }
    public WorkflowMapEntry(QName service, 
    						QName interfce, 
    						String endpoint, 
    						Definition wsdl) {
        mService = service;
        mInterface = interfce;
        mEndpointName = endpoint;
        mWsdl= wsdl;
        this.mType = EntryType.ENTRY_PROVIDE;
        mServiceUnitNames = new ArrayList<String> ();
    }

    public EntryType getEntryType() {
    	return this.mType;
    }
    
    public List<String> getServiceUnitNames() {
        return mServiceUnitNames;
    }
    
    
    public QName getInterface() {
        assert mInterface != null;
        return mInterface;
    }
    
    public boolean isTask() {
        return (this.mTaskModel != null)?true:false;
    }

    
    public void setService(QName service) {
        assert service != null;
        assert mService == null; // can only set once
        mService = service;
    }
    
    
    public QName getService() {
        return mService;
    }
    
    public String getEndpoint() {
    	return this.mEndpointName;
    }

    public void setStarted(boolean started) {
        mStarted = started;
    }
    
    public boolean isStarted() {
        return mStarted;
    }
    
    public void setServiceEndpoint(ServiceEndpoint endpointRef) {
        mServiceEndpoint = endpointRef;
    }
    
    public ServiceEndpoint getServiceEndpoint() {
        return mServiceEndpoint;
    }
        
    public void setEndpointStatus(String endpointStatusId, EndpointStatus endpointStatus) {
        mEndpointStatusId = endpointStatusId;
        mEndpointStatus = endpointStatus;
    }
    public String getEndpointStatusId() {
        return mEndpointStatusId;
    }
    public EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }



    public Definition getWsdl() {
        return mWsdl;
    }

    public void setWsdl(Definition wsdl) {
        this.mWsdl = wsdl;
    }
    public Task getTaskModel() {
        return mTaskModel;
    }
    public Element getXform() {
        return mXform;
    }
    public void setXform(Element inputXform) {
        mXform = inputXform;
    }

    public void setInputXformInstance(Element inputXformInstance) {
        // TODO Auto-generated method stub
        mInputXformInstance = inputXformInstance;
    }
    public void setOutputXformInstance(Element outputXformInstance) {
        // TODO Auto-generated method stub
        mOutputXformInstance = outputXformInstance;
    }
    
    public Element getOutputXformInstance() {
        return mOutputXformInstance;
    }

    public Element getInputXformInstance() {
        return mInputXformInstance;
    }
    
    public void setOperation(String operation) {
    	this.mOperation = operation;
    }
    
    public String getOperation() {
    	return this.mOperation;
    }
    
    public enum EntryType {
    	
    	ENTRY_PROVIDE("Service Provided by WLM"),
    	ENTRY_CONSUME("Service Consumed by WLM");
    	
    	private String mType;
    	
    	EntryType(String type) {
    		this.mType = type;
    	}
    	
    	public String toString() {
    		return this.mType;
    	}
    	
    }
}
