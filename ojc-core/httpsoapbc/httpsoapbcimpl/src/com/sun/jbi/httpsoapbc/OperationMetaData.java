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
 * @(#)OperationMetaData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.Message;
import javax.wsdl.Definition;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap12.SOAP12Body;
import javax.xml.namespace.QName;

/**
 *
 */
public class OperationMetaData {
    
    private String soapActionURL;
    /** QName of the input message */
    private QName inMsgQName;
    /** QName of the output message */
    private QName outMsgQName;
    /** Optional name defined for the input message, unique within the porttype */
    private String inMsgName;
    /** Optional name defined for the output message, unique within the porttype */
    private String outMsgName;
    
    private Map faults;
    private String operationName;
    private BindingInput bindingInput;
    private BindingOutput bindingOutput;
    private Message inputMessage;
    private Message outputMessage;
    
    private SOAPBody inputSoapBody;
    private SOAPBody outputSoapBody;
    
    private SOAP12Body inputSoap12Body;
    private SOAP12Body outputSoap12Body;
    private boolean isSoap12=false;
    
    private List inputSoapHeaders;
    private List outputSoapHeaders;
    
    // the default mode is document
    private boolean isDocumentMode = true;
    
    private String messageExchangePattern;
    
    private boolean useSoapAction;
    private boolean useMsgAsID;
    
    private Definition fullDefinition;
    
    // Configure the behavior of one-way processing
    private boolean oneWayReplyAfterDone = true;
    
    private Set cachedInputPartNames;
    
    //HTTP GET&POST BINDING
    private String httpOperationLocation;

    public static final String HTTP_URL_ENCODING_REPLACEMENT="urlReplacement";  // NOI18N
    public static final String HTTP_URL_ENCODING_ENCODED="urlEncoded";          // NOI18N
    public static final String HTTP_URL_ENCODING_UNSPECIFIED="unspecified";          // NOI18N
    private String httpUrlEncoding;
    
    private List inputMimeContents;
    private List outputMimeContents;
    private List cachedInputPartNameList;
    private Map soapInputMimeContents;
    private Map soapOutputMimeContents;
    
    /** Creates a new instance of OperationMetaData */
    public OperationMetaData() {
        setFaults(new HashMap());
    }

    public String getSoapActionURL() {
        return soapActionURL;
    }

    public void setSoapActionURL(String soapActionURL) {
        this.soapActionURL = soapActionURL;
    }

    public QName getInMsgQName() {
        return inMsgQName;
    }

    public void setInMsgQName(QName inMsgQName) {
        this.inMsgQName = inMsgQName;
    }

    public QName getOutMsgQName() {
        return outMsgQName;
    }

    public void setOutMsgQName(QName outMsgQName) {
        this.outMsgQName = outMsgQName;
    }


    public String getInMsgName() {
        return inMsgName;
    }

    public void setInMsgName(String inMsgName) {
        this.inMsgName = inMsgName;
    }

    public String getOutMsgName() {
        return outMsgName;
    }

    public void setOutMsgName(String outMsgName) {
        this.outMsgName = outMsgName;
    }
    
    
    public Map getFaults() {
        return faults;
    }

    public void setFaults(Map faults) {
        this.faults = faults;
    }

    public String getOperationName() {
        return operationName;
    }

    public void setOperationName(String operationName) {
        this.operationName = operationName;
    }

    public BindingInput getBindingInput() {
        return bindingInput;
    }

    public void setBindingInput(BindingInput bindingInput) {
        this.bindingInput = bindingInput;
    }

    public BindingOutput getBindingOutput() {
        return bindingOutput;
    }

    public void setBindingOutput(BindingOutput bindingOutput) {
        this.bindingOutput = bindingOutput;
    }

    public Message getInputMessage() {
        return inputMessage;
    }

    public void setInputMessage(Message inputMessage) {
        this.inputMessage = inputMessage;
    }

    public Message getOutputMessage() {
        return outputMessage;
    }

    public void setOutputMessage(Message outputMessage) {
        this.outputMessage = outputMessage;
    }

    public SOAPBody getInputSoapBody() {
        return inputSoapBody;
    }

    public void setInputSoapBody(SOAPBody inputSoapBody) {
        this.inputSoapBody = inputSoapBody;
    }

    public SOAPBody getOutputSoapBody() {
        return outputSoapBody;
    }

    public void setOutputSoapBody(SOAPBody outputSoapBody) {
        this.outputSoapBody = outputSoapBody;
    }

    public boolean isDocumentMode() {
        return isDocumentMode;
    }

    public void setIsDocumentMode(boolean isDocumentMode) {
        this.isDocumentMode = isDocumentMode;
    }

    public List getInputSoapHeaders() {
        return inputSoapHeaders;
    }

    public void setInputSoapHeaders(List inputSoapHeader) {
        this.inputSoapHeaders = inputSoapHeader;
    }

    public List getOutputSoapHeaders() {
        return outputSoapHeaders;
    }

    public void setOutputSoapHeaders(List outputSoapHeader) {
        this.outputSoapHeaders = outputSoapHeader;
    }

    public String getMessageExchangePattern() {
        return messageExchangePattern;
    }

    public void setMessageExchangePattern(String messageExchangePattern) {
        this.messageExchangePattern = messageExchangePattern;
    }    

    public boolean useSoapAction() {
        return useSoapAction;
    }

    public void setUseSoapAction(boolean useSoapAction) {
        this.useSoapAction = useSoapAction;
    }

    public boolean useMsgAsID() {
        return useMsgAsID;
    }

    public void setUseMsgAsID(boolean useMsgAsID) {
        this.useMsgAsID = useMsgAsID;
    }

    public Definition getFullDefinition() {
        return fullDefinition;
    }

    public void setFullDefinition(Definition fullDefinition) {
        this.fullDefinition = fullDefinition;
    }
    
    /**
     * Configures the behavior of one-way processing
     * @return true if BC replies to one-way (inonly) requests only after complete request was processed
     *         false if BC replies to one-way (inonly) requests as soon as the request was received
     */
    public boolean getOneWayReplyAfterProcessing() {
        return oneWayReplyAfterDone;
    }

    /**
     * Configures the behavior of one-way processing
     * @param replyAfterDone true if BC replies to one-way (inonly) requests only after complete request was processed
     *         false if BC replies to one-way (inonly) requests as soon as the request was received
     */    
    public void setOneWayReplyAfterProcessing(boolean replyAfterDone) {
        this.oneWayReplyAfterDone = replyAfterDone;
    }
    
    public Set getCachedInputPartNames() {
        return cachedInputPartNames;
    }

    public void setCachedInputPartNames(Set cachedInputPartNames) {
        this.cachedInputPartNames = cachedInputPartNames;
    }

    public String getHttpOperationLocation() {
        return httpOperationLocation;
    }

    public void setHttpOperationLocation(String httpOperationLocation) {
        this.httpOperationLocation = httpOperationLocation;
    }

    public List getInputMimeContents() {
        return inputMimeContents;
    }

    public void setInputMimeContents(List inputMimeContents) {
        this.inputMimeContents = inputMimeContents;
    }

    public List getOutputMimeContents() {
        return outputMimeContents;
    }

    public void setOutputMimeContents(List outputMimeContents) {
        this.outputMimeContents = outputMimeContents;
    }

    public String getHttpUrlEncoding() {
        return httpUrlEncoding;
    }

    public void setHttpUrlEncoding(String httpUrlEncoding) {
        this.httpUrlEncoding = httpUrlEncoding;
    }

    public List getCachedInputPartNameList() {
        return cachedInputPartNameList;
    }

    public void setCachedInputPartNameList(List cachedInputPartNameList) {
        this.cachedInputPartNameList = cachedInputPartNameList;
    }

    /**
     * @return the inputSoap12Body
     */
    public SOAP12Body getInputSoap12Body() {
        return inputSoap12Body;
    }

    /**
     * @param inputSoap12Body the inputSoap12Body to set
     */
    public void setInputSoap12Body(SOAP12Body inputSoap12Body) {
        this.inputSoap12Body = inputSoap12Body;
    }

    /**
     * @return the outputSoap12Body
     */
    public SOAP12Body getOutputSoap12Body() {
        return outputSoap12Body;
    }

    /**
     * @param outputSoap12Body the outputSoap12Body to set
     */
    public void setOutputSoap12Body(SOAP12Body outputSoap12Body) {
        this.outputSoap12Body = outputSoap12Body;
    }

    /**
     * @return the isSoap12
     */
    public boolean isSoap12() {
        return isSoap12;
    }

    /**
     * @param isSoap12 the isSoap12 to set
     */
    public void setSoap12(boolean isSoap12) {
        this.isSoap12 = isSoap12;
    }

    /** Set mime parts for SOAP
      * @param the map containing the mime parts for SOAP input message
     **/
    public void setInputSOAPMimeContents(Map soapMimeContents) {
        this.soapInputMimeContents = soapMimeContents;
    }
    
    /** Get mime parts for SOAP
     * @return the map containing mime parts for SOAP input message
     */
    public Map getInputSOAPMimeContents() {
        return this.soapInputMimeContents;
    }
    
    /** Set mime parts for SOAP
      * @param the map containing the mime parts for SOAP output message
     **/
    public void setOutputSOAPMimeContents(Map soapMimeContents) {
        this.soapOutputMimeContents = soapMimeContents;
    }

    
    /** Get mime parts for SOAP
     * @return the map containing mime parts for SOAP output message
     */
    public Map getOutputSOAPMimeContents() {
        return this.soapOutputMimeContents;
    }
    
}
