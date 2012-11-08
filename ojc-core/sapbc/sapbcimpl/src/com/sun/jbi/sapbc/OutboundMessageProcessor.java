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
 * @(#)OutboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sap.mw.jco.IFunctionTemplate;
import com.sap.mw.jco.JCO;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Endpoint.EndpointMessageType;
import com.sun.jbi.sapbc.Endpoint.EndpointState;
import com.sun.jbi.sapbc.extensions.SAPFmOperation;
import com.sun.jbi.sapbc.extservice.SAPBCClient;
import com.sun.jbi.sapbc.extservice.SAPException;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLMessage;
import java.io.FileOutputStream;
import java.net.URI;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * This class processes request and reply
 * messages received from the SE.
 *
 * @author Sherry Weng
 */
public class OutboundMessageProcessor implements Runnable {
    public OutboundMessageProcessor(
            DeliveryChannel channel,
            Map<String, ServiceUnit> serviceUnits) {
        
        mChannel = channel;
        mServiceUnits = serviceUnits;
        mLogger = Messages.getLogger(OutboundMessageProcessor.class);
    }
    
    // TODO: coupling with inbound functionality not yet available
    /*
    public OutboundMessageProcessor(DeliveryChannel channel,
                                    Map serviceUnits,
                                    Map inboundMessageExchanges) {
        mChannel = channel;
        mServiceUnits = serviceUnits;
        mInboundExchanges = inboundMessageExchanges;
        mMonitor = new Object();
     
        mLogger = Messages.getLogger(OutboundMessageProcessor.class);
    }
     */
    
    /**
     * Entry point to execute this thread
     * to handle the message exchanges.
     */
    public void run() {
        MessageExchange exchange = null;
        
        try {
            isStopped = false;
            while (!isStopped) {
                exchange = mChannel.accept(CHANNEL_ACCEPT_WAIT);
                if (exchange != null) {
                    final String id = exchange.getExchangeId();
                    final URI pattern = exchange.getPattern();
                    
                    Endpoint endpoint = findEndpoint(exchange);
                    if (endpoint == null) {
                        String errmsg = mMessages.getString(
                                "OutboundMessageProcessor.Failed_endpoint_match",
                                new Object[] {
                            exchange.getEndpoint().getServiceName(),
                            exchange.getEndpoint().getEndpointName()
                        }
                        );
                        Exception e = new Exception(errmsg);
                        exchange.setError(e);
                        throw e;
                    }
                    
                    if (exchange.getStatus() == ExchangeStatus.DONE) {
                        endpoint.getEndpointStatus().incrementReceivedDones();
                        continue;
                    }
                    
                    if (exchange.getStatus() == ExchangeStatus.ERROR) {
                        endpoint.getEndpointStatus().incrementReceivedErrors();
                        continue;
                    }
                    
                    EndpointState status = endpoint.getState();
                    if (status != EndpointState.RUNNING) {
                        // Just ignore the message if the endpoint is not in the
                        // running state (i.e. stopped or shutdown).
                        if (mLogger.isLoggable(Level.INFO)) {
                            String strState;
                            switch (status) {
                                case STOPPED:
                                    strState = mMessages.getString("OutboundMessageProcessor.State_STOPPED");
                                    break;
                                case SHUTDOWN:
                                    strState = mMessages.getString("OutboundMessageProcessor.State_SHUTDOWN");
                                    break;
                                default:
                                    throw new RuntimeException(mMessages.getString("OutboundMessageProcessor.Indeterminate_endpoint_state"));
                            }
                            mLogger.log(
                                    Level.INFO,
                                    "OutboundMessageProcessor.Failed_request_not_running",
                                    new Object[] {
                                endpoint.getEndpointName(),
                                strState,
                                id,
                            });
                        }
                        continue;
                    }
                    
                    String strPattern = pattern.toString().trim();
                    switch (ExchangePattern.valueOf(exchange)) {
                    case IN_ONLY:
                        processInOnly((InOnly) exchange, endpoint);
                        break;
                    case IN_OUT:
                        processInOut((InOut) exchange, endpoint);
                        mChannel.send(exchange);
                        break;
                    case IN_OPTIONAL_OUT:
                        mLogger.log(
                                Level.WARNING,
                                "OutboundMessageProcessor.Unsupported_exchange_pattern_in_optional_out",
                                id);
                        break;
                    case ROBUST_IN_ONLY:
                        mLogger.log(
                                Level.WARNING,
                                "OutboundMessageProcessor.Unsupported_exchange_pattern_in_robust",
                                id);
                        break;
                    default:
                        mLogger.log(
                                Level.SEVERE,
                                "OutboundMessageProcessor.Indeterminate_exchange_pattern",
                                new Object[] { strPattern, id, }
                        );
                        break;
                    }
                }
            }
        } catch (MessagingException ex) {
            if (exchange != null) {
                String errmsg = mMessages.getString(
                        "OutboundMessageProcessor.Failed_in_out_exchange_status_set_error",
                        exchange.getExchangeId());
                mLogger.log(Level.SEVERE, errmsg, ex);
            } else {
                mLogger.log(Level.SEVERE,
                        mMessages.getString("OutboundMessageProcessor.Unexpected_exception", ex.getLocalizedMessage()),
                        ex);
            }
            
        } catch (Throwable ex) {
            mLogger.log(Level.SEVERE,
                    mMessages.getString("OutboundMessageProcessor.Unexpected_exception", ex.getLocalizedMessage()),
                    ex);
        }
        
        mLogger.log(Level.FINE, "OutboundMessageProcessor.Completed_processing");

   }
    
    public void stopReceiving() {
        mLogger.info("OutboundMessageProcessor.Stopped_thread");
        isStopped = true;
    }
    
    public void processInOnly(InOnly exchange, Endpoint endpoint) {
        try {
            mLogger.info("OutboundMessageProcessor.Processing_inonly");
            
            endpoint.getEndpointStatus().incrementReceivedRequests();
            String rfmName = "BAPI_FLIGHT_GETDETAIL";
            
            mLogger.log(Level.INFO, "Test 1 complete: JCO Function successfully acquired.");
            
            // Let's see what the normalized message looks like
            //_debug(exchange.getInMessage());
            
            mLogger.log(Level.INFO, "Test 2 complete: Normalized Message received and examined.");
            
            exchange.setStatus(ExchangeStatus.DONE);
            endpoint.getEndpointStatus().incrementSentDones();
/*
            String result = fc.getExportParameterList().toXML();
            mLogger.log(Level.FINE, "Result of Flight BAPI execution: ||sod||" + result + "||eod||");
 */
        } catch (MessagingException ex) {
            String errmsg = mMessages.getString(
                    "OutboundMessageProcessor.Failed_in_only_exchange_status_set_error",
                    exchange.getExchangeId());
            mLogger.log(Level.SEVERE, errmsg, ex);
        }
    }
    
    public void processInOut(InOut exchange, Endpoint endpoint) {
        final WSDLDefinitions wsdlDefinition = endpoint.getDefinition();
        final QName opQName = exchange.getOperation();
        final QName endpointQName = new QName("",endpoint.getEndpointName());
        final Binding wsdlBinding = SAPWSDLUtilities.findBindingDefinition(wsdlDefinition, endpoint.getServiceName(), endpointQName);
        final PortType wsdlPortType = wsdlBinding.getWSDLPortType();
        Collection wsdlOperations = wsdlPortType.getOperations(opQName.getLocalPart());
        Operation[] wsdlOperationsArray = (Operation[]) wsdlOperations.toArray(new Operation[wsdlOperations.size()]);
        /***
         * Note: This assumes that all operation names are unigue which is currently supported by
         * the Netbeans WSDL Editor
         */
        final Operation wsdlOperation = wsdlOperationsArray[0];
        final WSDLMessage outWSDLMsg = wsdlOperation.getOutput().getWSDLMessage();
        final WSDLMessage inWSDLMsg = wsdlOperation.getInput().getWSDLMessage();
        
        JCO.Request req = null;
        JCO.Response resp = null;
        
        try {
            mLogger.info("OutboundMessageProcessor.Processing_inout");
            
            endpoint.getEndpointStatus().incrementReceivedRequests();
            
            try {
                req = createJcoRequest(endpoint, opQName);
            } catch (SAPException se) {
                String errmsg = mMessages.getString(
                        "OutboundMessageProcessor.Failed_create_jco_request",
                        new Object[] {opQName.toString(), exchange.getExchangeId()});
                mLogger.log(Level.SEVERE, errmsg, se);
                endpoint.getEndpointStatus().incrementSentErrors();
                exchange.setError(se);
                exchange.setStatus(ExchangeStatus.ERROR);
                return;
            }
            if (req == null) {
                endpoint.getEndpointStatus().incrementSentErrors();
            }
            
            //Let's see what the normalized message looks like
            //_debug(exchange.getInMessage());
            
            // Verify operation name, exchange pattern, etc. before continuing
            validateExchange(opQName, endpoint);
            
            // Process the request.
            // The denormalized content is returned a series of one or more DOM nodes.
            MessageDenormalizer denormalizer = new MessageDenormalizer();
            List<Node> nodes = denormalizer.denormalize(
                    exchange.getInMessage(),
                    inWSDLMsg,
                    endpoint.getEndpointName());
            try {
                // Render the message(s) into SAP JCo API operations
                DOMJCOTransformer transformer = new DOMJCOTransformer(wsdlDefinition);
                for (Node node: nodes) {
                    //TODO: While there should be only one node this needs to be changed
                    //to the appropirate response for more than one node.
                    SAPWSDLUtilities.nodetostring(node);
                    transformer.transform(req, node);
                }
                
                resp = executeRfm(endpoint, req);
                endpoint.getEndpointStatus().incrementSentDones();
            } catch (SAPException se) {
                String errmsg = mMessages.getString(
                        "OutboundMessageProcessor.Failed_execute_RFM",
                        new Object[] {req.getName(), exchange.getExchangeId()});
                mLogger.log(Level.SEVERE, errmsg, se);
                endpoint.getEndpointStatus().incrementSentErrors();
                exchange.setError(se);
                exchange.setStatus(ExchangeStatus.ERROR);
                return;
            }
            
            // Compose the response.
            if (ExchangeStatus.ACTIVE.equals(exchange.getStatus())) {
                // Generate the XML response from the JCO function data.
                //WSDLMessage msgdef = findOutputMessageDefinition(endpoint, wsdlDefinition, opQName);
                //exchange.getInMessage();
                JCODOMTransformer transformer = new JCODOMTransformer(wsdlDefinition);
                Document reply = transformer.transform(outWSDLMsg, resp);
                
                SAPWSDLUtilities.doctostring(reply);
/*
DOC XML CONTENT
<?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>
<sapns:FlightGetDetailResponse xmlns:sapns:="urn:sap-com:document:sap:soap:functions:mc-style">
    <sapns:AdditionalInfo>
        <sapns:Flighttime>30240</sapns:Flighttime>
        <sapns:Distance>7865.0000</sapns:Distance>
        <sapns:Unit>KM</sapns:Unit>
        <sapns:Unitiso>KMT</sapns:Unitiso>
        <sapns:Planetype>A319</sapns:Planetype>
        <sapns:Flighttype/>
    </sapns:AdditionalInfo>
    <sapns:Availibility>
        <sapns:Economax>350</sapns:Economax>
        <sapns:Econofree>347</sapns:Econofree>
        <sapns:Businmax>0</sapns:Businmax>
        <sapns:Businfree>0</sapns:Businfree>
        <sapns:Firstmax>0</sapns:Firstmax>
        <sapns:Firstfree>0</sapns:Firstfree>
    </sapns:Availibility>
    <sapns:ExtensionOut/>
    <sapns:FlightData>
        <sapns:Airlineid>LH</sapns:Airlineid>
        <sapns:Airline>Lufthansa</sapns:Airline>
        <sapns:Connectid>0400</sapns:Connectid>
        <sapns:Flightdate>1995-02-28</sapns:Flightdate>
        <sapns:Airportfr>FRA</sapns:Airportfr>
        <sapns:Cityfrom>FRANKFURT</sapns:Cityfrom>
        <sapns:Airportto>JFK</sapns:Airportto>
        <sapns:Cityto>NEW YORK</sapns:Cityto>
        <sapns:Deptime>10:10:00</sapns:Deptime>
        <sapns:Arrtime>11:34:00</sapns:Arrtime>
        <sapns:Arrdate>1995-02-28</sapns:Arrdate>
        <sapns:Price>899.0000</sapns:Price>
        <sapns:Curr>DEM</sapns:Curr>
        <sapns:Curriso>DEM</sapns:Curriso>
    </sapns:FlightData>
    <sapns:Return>
        <sapns:item>
            <sapns:Type>S</sapns:Type>
            <sapns:Id>BC_IBF</sapns:Id>
            <sapns:Number>000</sapns:Number>
            <sapns:Message>Method was executed successfully</sapns:Message>
            <sapns:Logno/>
            <sapns:Logmsgno>000000</sapns:Logmsgno>
            <sapns:Messagev1/>
            <sapns:Messagev2/>
            <sapns:Messagev3/>
            <sapns:Messagev4/>
            <sapns:Parameter/>
            <sapns:Row>0</sapns:Row>
            <sapns:Field/>
            <sapns:System>LSYS800</sapns:System>
        </sapns:item>
    </sapns:Return>
</sapns:FlightGetDetailResponse>
 */
                
                // Normalize the response and dispatch it.
                MessageNormalizer normalizer = new MessageNormalizer();
                NormalizedMessage normalizedMsg = exchange.createMessage();
                normalizer.normalize(normalizedMsg, outWSDLMsg, reply);
                exchange.setOutMessage(normalizedMsg);
                endpoint.getEndpointStatus().incrementSentReplies();
            }
        } catch (ParserConfigurationException ex) {
            String errmsg = mMessages.getString(
                    "OutboundMessageProcessor.Failed_transform_setup",
                    new Object[] {
                exchange.getExchangeId(),
                ex.getLocalizedMessage()
            });
            mLogger.log(Level.SEVERE, errmsg, ex);
            return;
        } catch (MessagingException ex) {
            String errmsg = mMessages.getString(
                    "OutboundMessageProcessor.Failed_in_out_exchange_status_set_error",
                    exchange.getExchangeId());
            mLogger.log(Level.SEVERE, errmsg, ex);
            return;
        } catch (MessageProcessingException ex) {
            String errmsg = mMessages.getString(
                    "OutboundMessageProcessor.Failed_in_out_validation",
                    exchange.getExchangeId());
            mLogger.log(Level.SEVERE, errmsg, ex);
        }
    }
    
    private JCO.Response executeRfm(Endpoint endpoint,JCO.Request req)
    throws SAPException {
        JCO.Response retResp = null;
        try {
            
            // nang: commenting out this call, we populate the function from actual data now
            //populateFlightGetDetail(fc);
            
            // Execute the BAPI
            retResp = getJcoClient(endpoint).execute(req);
            
            /* Debug
            mLogger.log(Level.INFO, "############debug response function name is ["+retResp.getName()+"]");
            if (req.getName().equals("BAPI_FLIGHT_GETDETAIL")){
                String extentionVal = req.getTable("EXTENSION_IN").getString("VALUEPART1");
                mLogger.log(Level.INFO, "EXTENSION_IN  VALUEPART1 ["+extentionVal+"]");
             
                String airline = retResp.getStructure("FLIGHT_DATA").getString("AIRLINE");
                String msg = retResp.getTable("RETURN").getString("MESSAGE");
                mLogger.log(Level.INFO, "AIRLINE ["+airline+"]");
                mLogger.log(Level.INFO, "MESSAGE ["+msg+"]");
            } else if (req.getName().equals("IDOC_INBOUND_ASYNCHRONOUS")) {
                //JCO.ParameterList tableParams = retResp.getTableParameterList();
                JCO.Table crTab = retResp.getTable("IDOC_CONTROL_REC_40");
                String docnum = crTab.getString("DOCNUM");
                String idoctype = crTab.getString("IDOCTYP");
                mLogger.log(Level.INFO, "DOCNUM ["+docnum+"]");
                mLogger.log(Level.INFO, "IDOCTYP ["+idoctype+"]");
            }
             **/
        } catch (com.sap.mw.jco.JCO.Exception je) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{je.getClass().getName(), "writeMessage()", je.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            throw new SAPException(je);
        } catch (Exception je) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{je.getClass().getName(), "writeMessage()", je.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            throw new SAPException(je);
        }
        return retResp;
    }
    
    /**
     * Create a SAP JCO.Client object to be used to send information to SAP
     */
    private JCO.Client getJcoClient(Endpoint endpoint)
    throws SAPException {
        if (mSAPClient == null) {
            // Get a handle to the SAP BAPI/RFC.
            /**
             * TODO: Make sure that the client is connected to the right endpoint
             * Also move this to a connection pool so we can keep multiple connections
             * per JVM.
             */
            try {
                mSAPClient = new SAPBCClient(endpoint).getClient().getJCOClient();
            } catch (Exception e) {
                String errMsg = mMessages.getString("OutboundMessageProcessor.Failed_sap_connect");
                mLogger.log(Level.SEVERE, errMsg);
                throw new SAPException(e);
            }
            
        }
        
        return mSAPClient;
    }
    
    /**
     * Create a SAP JCO.Request object to be used to send information to SAP
     */
    private JCO.Request createJcoRequest(Endpoint endpoint, QName opQName)
    throws SAPException {
        // Determine RFM name
        String rfmName = null;
        Object op = endpoint.getSAPOperations().get(opQName);
        if (op instanceof SAPFmOperation) {
            SAPFmOperation fmop = (SAPFmOperation) op;
            rfmName = fmop.functionName();
            mLogger.log(Level.INFO, "Instance of SAPFmOperation ["+fmop.functionName()+"]");
        } else {
            return null;
        }
        
        JCO.Request req = null;
        JCO.Repository sapRepository = new JCO.Repository("SAPBC", getJcoClient(endpoint));
        /***
         * TODO: Move this into some sort of factory so that we do not need to recreate
         * the SAP request (and response) each time a message comes through
         */
        IFunctionTemplate ft = sapRepository.getFunctionTemplate(rfmName);
        
        if (ft == null) {
            mLogger.log(Level.SEVERE, "Unable to find SAP RFM " + rfmName);
        } else {
            req = ft.getRequest();
            if (req == null) {
                mLogger.log(Level.SEVERE, "Unable to instantiate SAP RFM " + rfmName);
            }
        }
        return req;
    }
    
    private void populateFlightGetDetail(JCO.Function rfcFunction) {
        try {
            mLogger.log(Level.INFO, "############In executeFlightGetDetail ["+rfcFunction.getName()+"]");
            
            String airline_id = "LH";
            String connection_id = "0400";
            //String flight_date = "28.02.1995";
            java.util.Calendar calObj = java.util.Calendar.getInstance();
            calObj.set( 1995, 2 - 1, 28 );
            java.util.Date flight_date = calObj.getTime();
            
            JCO.ParameterList importParams = null;
            JCO.ParameterList exportParams = null;
            JCO.ParameterList tableParams = null;
            
            importParams = rfcFunction.getImportParameterList();
            
            importParams.setValue(airline_id,"AIRLINEID");
            importParams.setValue(connection_id,"CONNECTIONID");
            importParams.setValue(flight_date,"FLIGHTDATE");
            mLogger.log(Level.INFO, "Executing Flight with the following values Airline ID [" + airline_id + "] Connection ID [" + connection_id + "] Flight date [" + calObj.MONTH + '-' + calObj.DATE + '-' + calObj.YEAR + "]" );
            
            JCO.Table extInTab = rfcFunction.getTableParameterList().getTable("EXTENSION_IN");
            extInTab.appendRows(3);
            extInTab.setRow(0);
            extInTab.setValue("STRUCTURE1", "STRUCTURE");
            extInTab.setValue("VP1", "VALUEPART1");
            extInTab.setRow(1);
            extInTab.setValue("STRUCTURE2", "STRUCTURE");
            extInTab.setValue("VP2", "VALUEPART1");
            extInTab.setRow(2);
            extInTab.setValue("STRUCTURE3", "STRUCTURE");
            extInTab.setValue("VP3", "VALUEPART1");
            tableParams = rfcFunction.getTableParameterList();
            
            //mLogger.info("Before execute["+rfcFunction.toString()+"]");
            
            if (importParams != null)
                mLogger.log(Level.INFO, "Before  execute IMPORT ["+importParams.toXML()+"]");
            if (tableParams != null)
                mLogger.log(Level.INFO, "Before execute TABLES ["+tableParams.toXML()+"]");
            
            mLogger.info( "End of executeFlightGetDetail function." );
            
        }catch (Exception ex) {
            ex.printStackTrace();
            mLogger.info("*** Exception caught ["+ex.getMessage()+"]");
            return;
        }
    }
    
    private void validateExchange(final QName operationName, final Endpoint endpoint)
    throws MessageProcessingException {
        mLogger.fine("validatingExchange for operationName ["+operationName.toString()+"] with endpoint ["+endpoint.getEndpointName()+"]");
        
        final Map patterns = endpoint.getOperationMsgExchangePattern();
        final Map ops = endpoint.getSAPOperations();
        
        if (ops == null || ops.get(operationName) == null) {
            throw new MessageProcessingException(
                    mMessages.getString(
                    "OutboundMessageProcessor.Invalid_operation",
                    new Object[] { endpoint.getEndpointName(), operationName.toString() }
            )
            );
        }
        
        if (patterns == null) {
            throw new MessageProcessingException(
                    mMessages.getString(
                    "OutboundMessageProcessor.Invalid_mep",
                    new Object[] { endpoint.getEndpointName(), operationName.toString() }
            )
            );
        }
        
        String mep = (String) patterns.get(operationName);
        if (mep == null || EndpointMessageType.UNSUPPORTED.equals(mep)) {
            throw new MessageProcessingException(
                    mMessages.getString(
                    "OutboundMessageProcessor.Invalid_mep",
                    new Object[] { String.valueOf(mep), operationName.toString() }
            )
            );
        }
    }
    
    private void _debug(NormalizedMessage msg) {
        mLogger.log(Level.INFO, "Message toString: |(begin)|" + msg.toString() + "|(end)|");
        
        mLogger.log(Level.INFO, "Attachment names: (list)");
        for (Object name : msg.getAttachmentNames()) {
            mLogger.log(Level.INFO, " -> " + name.toString());
        }
        
        mLogger.log(Level.INFO, "Properties: (list)");
        for (Object prop : msg.getPropertyNames()) {
            mLogger.log(
                    Level.INFO,
                    " name: |" + prop.toString() + "|   "
                    + "value: |" + msg.getProperty(prop.toString())+ "|");
        }
        
        mLogger.log(Level.INFO, "Message content system ID: " + msg.getContent().getSystemId());
        TransformerFactory transfact = TransformerFactory.newInstance();
        Transformer transer;
        try {
            FileOutputStream fos = new FileOutputStream("C:/sapbc.dump");
            transer = transfact.newTransformer();
            StreamResult result = new StreamResult(fos);
            Source src = msg.getContent();
            transer.transform(src, result);
            fos.flush();
            fos.close();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, ex.getLocalizedMessage(), ex);
        }
    }
    
    protected Endpoint findEndpoint(MessageExchange msgExchange) {
        Endpoint endpoint = null;
        for (Iterator it = mServiceUnits.values().iterator(); it.hasNext();) {
            for (Iterator it2 = ((ServiceUnit)it.next()).getEndpoints().iterator(); it2.hasNext();) {
                Endpoint aEndPoint = (Endpoint)it2.next();
                QName serviceName = msgExchange.getEndpoint().getServiceName();
                String endpointName = msgExchange.getEndpoint().getEndpointName();
                
                if (aEndPoint.getServiceName().equals(serviceName) &&
                        aEndPoint.getEndpointName().equals(endpointName)) {
                    endpoint = aEndPoint;
                }
            }
        }
        
        return endpoint;
    }
    
    private static final Messages mMessages =
            Messages.getMessages(OutboundMessageProcessor.class);
    
    private static final long CHANNEL_ACCEPT_WAIT = 5000L;
    
    private static Logger mLogger;
    
    private DeliveryChannel mChannel;
    
// TODO: coupling with inbound functionality not yet available
//private Map mInboundExchanges;
    private Map<String, ServiceUnit> mServiceUnits;
    private Object mMonitor;
    
    private volatile boolean isStopped = true;
    
    private JCO.Client mSAPClient = null;
    
}
