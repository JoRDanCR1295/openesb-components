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
 * @(#)EndpointImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
//import java.util.logging.Logger;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.jbi.eManager.provider.EndpointStatus;
//import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.extensions.SwiftAddress;
import com.sun.jbi.swiftbc.extensions.SwiftBinding;
import com.sun.jbi.swiftbc.extensions.SwiftInput;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.swiftbc.extensions.SwiftOutput;
import com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties;

public class EndpointImpl implements Endpoint, Serializable
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private QName mServiceName;

	private String mEndpointName;

	private Definition mDefinition;

	private int mEndpointState;

	private EndpointStatus mEndpointStatus;

	private int mEndpointType;

	private ServiceEndpoint mServiceEndpoint;

	private Document mServiceDescription;

	private String mServiceUnitPath;

	private SwiftAddress mSwiftAddress;

	private SwiftBinding mSwiftBinding;

	private SwiftProtocolProperties mSwiftProtocolProperties;

	private Map<QName, SwiftOperation> mOperations;

	private Map<SwiftOperation, SwiftInput> mOperationInput;

	private Map<SwiftOperation, SwiftOutput> mOperationOutput;

	private Map mPartMapping;

	private Map mOperationMEPs;

	private List mXsds;

	//private static Logger logger = Messages.getLogger(EndpointImpl.class);


	public EndpointImpl()
	{
		mOperations = new HashMap<QName, SwiftOperation>();
		mOperationInput = new HashMap<SwiftOperation, SwiftInput>();
		mOperationOutput = new HashMap<SwiftOperation, SwiftOutput>();
	}

	// //////
	//
	// Binding and Port Information Methods
	//
	// //////

	public QName getServiceName()
	{
		return mServiceName;
	}

	public void setServiceName(QName serviceName)
	{
		mServiceName = serviceName;
	}

	public String getEndpointName()
	{
		return mEndpointName;
	}

	public void setEndpointName(String endpointName)
	{
		mEndpointName = endpointName;
	}

	public Definition getDefinition()
	{
		return mDefinition;
	}

	public void setDefinition(Definition definition)
	{
		mDefinition = definition;
	}

	// //////
	//
	// State Information Methods
	//
	// //////

	public int getState()
	{
		return mEndpointState;
	}

	public void setState(int state)
	{
		mEndpointState = state;
	}

	public void setEndpointStatus(EndpointStatus val)
	{
		mEndpointStatus = val;
	}

	public EndpointStatus getEndpointStatus()
	{
		return mEndpointStatus;
	}

	// //////
	//
	// Message Exchange Pattern Methods
	//
	// //////

	public int getEndpointType()
	{
		return mEndpointType;
	}

	public void setEndpointType(int type)
	{
		mEndpointType = type;
	}

	// //////
	//
	// JBI-representation of Endpoint
	//
	// //////

	public ServiceEndpoint getServiceEndpoint()
	{
		return mServiceEndpoint;
	}

	public void setServiceEndpoint(ServiceEndpoint serviceEndpoint)
	{
		mServiceEndpoint = serviceEndpoint;
	}

	public Document getServiceDescription()
	{
		return mServiceDescription;
	}

	public void setServiceDescription(Document serviceDescription)
	{
		mServiceDescription = serviceDescription;
	}

	public void setServiceUnitPath(String serviceUnitPath)
	{
		mServiceUnitPath = serviceUnitPath;
	}

	public String getServiceUnitPath()
	{
		return mServiceUnitPath;
	}

	// //////
	//
	// Swift-specific Endpoint. These should probably be refactored
	// out in a better way so as to maintain their original
	// hierarchical structure
	//
	// //////

	public SwiftAddress getSwiftAddress()
	{
		return mSwiftAddress;
	}

	public void setSwiftAddress(SwiftAddress address)
	{
		mSwiftAddress = address;
	}

	/**
	 * 
	 * @return
	 */
	public SwiftProtocolProperties getSwiftProtocolProperties()
	{
		return mSwiftProtocolProperties;
	}

	public void setSwiftProtocolProperties(
			SwiftProtocolProperties protocolProperties)
	{
		mSwiftProtocolProperties = protocolProperties;
	}

	public SwiftBinding geSwiftBinding()
	{
		return mSwiftBinding;
	}

	public void setSwiftBinding(SwiftBinding binding)
	{
		mSwiftBinding = binding;
	}

	public Map getSwiftOperations()
	{
		return mOperations;
	}

	public void setSwiftOperations(Map<QName, SwiftOperation> operations)
	{
		mOperations = operations;
	}

	public SwiftInput getSwiftOperationInput(SwiftOperation operation)
	{
		return (SwiftInput) mOperationInput.get(operation);
	}

	public void setSwiftOperationInput(SwiftOperation operation,
			SwiftInput operationInput)
	{
		mOperationInput.put(operation, operationInput);
	}

	public SwiftOutput getSwiftOperationOutput(SwiftOperation operation)
	{
		return (SwiftOutput) mOperationOutput.get(operation);
	}

	public void setSwiftOperationOutput(SwiftOperation operation,
			SwiftOutput operationOutput)
	{
		mOperationOutput.put(operation, operationOutput);
	}

	public void setOperationMsgExchangePattern(Map opMEPs)
	{
		mOperationMEPs = opMEPs;
	}

	public Map getOperationMsgExchangePattern()
	{
		return mOperationMEPs;
	}

	public void setMessagePartEncoderMapping(Map partMapping)
	{
		mPartMapping = partMapping;
	}

	public Map getMessagePartEncoderMapping()
	{
		return mPartMapping;
	}

	public void setXsdsList(List xsds)
	{
		mXsds = xsds;
	}

	public List getXsdsList()
	{
		return mXsds;
	}

	public SwiftBinding getSwiftBinding()
	{
		return mSwiftBinding;
	}


}
