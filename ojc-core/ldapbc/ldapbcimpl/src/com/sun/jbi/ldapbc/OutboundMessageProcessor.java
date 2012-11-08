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
package com.sun.jbi.ldapbc;

import java.io.IOException;
import java.net.URI;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.*;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jbi.JBIException;
import javax.naming.NamingException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.naming.NameAlreadyBoundException;
import javax.naming.NameNotFoundException;
import javax.naming.NamingEnumeration;
import javax.naming.NoPermissionException;
import javax.naming.PartialResultException;
import javax.naming.ReferralException;
import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttribute;
import javax.naming.directory.DirContext;
import javax.naming.directory.InvalidAttributeIdentifierException;
import javax.naming.directory.InvalidSearchControlsException;
import javax.naming.directory.ModificationItem;
import javax.naming.directory.SchemaViolationException;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import javax.naming.ldap.Control;
import javax.naming.ldap.LdapContext;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.PagedResultsControl;
import javax.naming.ldap.PagedResultsResponseControl;
import javax.naming.ldap.SortControl;
import javax.naming.ldap.SortKey;
import javax.naming.ldap.SortResponseControl;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.ldapbc.add.LDAPAddResponse;
import com.sun.jbi.ldapbc.delete.DeleteEntry;
import com.sun.jbi.ldapbc.delete.LDAPDelete;
import com.sun.jbi.ldapbc.delete.LDAPDeleteResponse;
import com.sun.jbi.ldapbc.extensions.LDAPOperation;
import com.sun.jbi.ldapbc.extensions.LDAPOperationAbstract;
import com.sun.jbi.ldapbc.extensions.LDAPResponseAbstract;
import com.sun.jbi.ldapbc.add.LDAPAdd;
import com.sun.jbi.ldapbc.update.LDAPUpdate;
import com.sun.jbi.ldapbc.update.LDAPUpdateResponse;
import com.sun.jbi.ldapbc.update.UpdateBean;
import com.sun.jbi.ldapbc.update.UpdateBeanAdd;
import com.sun.jbi.ldapbc.update.UpdateBeanRemove;
import com.sun.jbi.ldapbc.update.UpdateBeanRemoveAll;
import com.sun.jbi.ldapbc.update.UpdateBeanReplace;
import com.sun.jbi.ldapbc.util.LdapConnection;


/**
 *
 * Process replies/requests received from the SE.
 */
public class OutboundMessageProcessor implements Runnable {

    private static final Messages mMessages = Messages.getMessages(OutboundMessageProcessor.class);
    private static final Logger mLogger = Messages.getLogger(OutboundMessageProcessor.class);
    Map mEndpoints;
    DocumentBuilder mDocBuilder;
    private DeliveryChannel mChannel;
    private MessageExchange mExchange;
    private ComponentContext mContext;
    private Map mInboundExchanges;
    private static final String CLIENT = "Client";
    private static final String SERVER = "Server";
    private LdapConnection conn;
    private RuntimeConfiguration mRtCfg;
    private LDAPSearch search;
    private LDAPAdd add;
    private KeyStoreUtilClient mKeyStoreUtil;

    public OutboundMessageProcessor(final DeliveryChannel chnl,
            final MessageExchange exchange, final Map endpoints, final ComponentContext context,
            final Map inboundMessageExchanges, RuntimeConfiguration runtimeConfig) throws ParserConfigurationException {
        mChannel = chnl;
        mEndpoints = endpoints;
        mExchange = exchange;
        mContext = context;
        mInboundExchanges = inboundMessageExchanges;
        mRtCfg = runtimeConfig;
        search = new LDAPSearch();
        add = new LDAPAdd();

        final DocumentBuilderFactory docBuilderFact = DocumentBuilderFactory.newInstance();
        mDocBuilder = docBuilderFact.newDocumentBuilder();
        mKeyStoreUtil = new KeyStoreUtilClient(context);
    }

    /**
     *
     */
    public void run() {
        if (OutboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
            OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00411.OMP_Accept_msg", new Object[]{mExchange.getExchangeId()}));
        }

        try {
            execute();
        } catch (final Exception ex) {
            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
                    OutboundMessageProcessor.mMessages.getString("LDAPBC-E00403.OMP_Unexpected_exception",
                    new Object[]{ex.getLocalizedMessage(), ex}));
        }

        if (OutboundMessageProcessor.mLogger.isLoggable(Level.FINE)) {
            OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-E00404.OMP_Complete_processing"));
        }
    }

    /**
     * Process the message exchange
     */
    public void execute() {
        if (mExchange != null) {
            final String exchangeId = mExchange.getExchangeId();

            if (OutboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
                OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00411.OMP_Accept_msg", new Object[]{exchangeId}));
            }

            final boolean inbound = mInboundExchanges.containsKey(exchangeId);
            final ListenerMeta listenerMeta = (ListenerMeta) mInboundExchanges.get(exchangeId);
            MessageExchangeReplyListener listener = null;

            if (listenerMeta != null) {
                listener = listenerMeta.getMessageExchangeReplyListener();
            }

            if (inbound) {
                final long invocationTime = listenerMeta.getRequestInvocationTime();

                if (OutboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
                    final long difference = System.currentTimeMillis() -
                            invocationTime;
                    OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00412.OMP_Resp_Ex",
                            new Object[]{exchangeId, difference}));
                }
            }

            final URI pattern = mExchange.getPattern();

            if (OutboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
                OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00413.OMP_Pattern",
                        new Object[]{exchangeId, pattern}));
            }

            final String serviceName = mExchange.getEndpoint().getServiceName().toString();
            final String endpointName = mExchange.getEndpoint().getEndpointName();

            if (OutboundMessageProcessor.mLogger.isLoggable(Level.FINE)) {
                OutboundMessageProcessor.mLogger.fine("Gettin bean for " + serviceName + endpointName);
            }

            String epName = null;

            if (inbound) {
                epName = EndpointImpl.getUniqueName(serviceName, endpointName,
                        EndpointImpl.ENDPOINT_TYPE_INBOUND);
            } else {
                epName = EndpointImpl.getUniqueName(serviceName, endpointName,
                        EndpointImpl.ENDPOINT_TYPE_OUTBOUND);
            }

            final EndpointImpl epb = (EndpointImpl) mEndpoints.get(epName);
            final String status = epb.getValue(EndpointImpl.STATUS);

            if (status != EndpointImpl.STATUS_RUNNING) {
                // If the endpoint is not in the RUNNING state (i.e. is stopped
                // or
                // shutdown), ignore the message
                if (OutboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
                    OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00414.OMP_EP_state"));
                }
            } else {
                final String pat = pattern.toString().trim();

                if (pat.equals(MEP.IN_OUT.toString())) {
                    OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00415.OMP_Recv_InOut",
                            new Object[]{mExchange.getExchangeId()}));
                    processInOut((InOut) mExchange, epb);
                } else if (pat.equals(MEP.IN_ONLY.toString())) {
                    OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00416.OMP_Recv_InOnly",
                            new Object[]{mExchange.getExchangeId()}));

                    if (inbound) {
                        processInOnlyInbound((InOnly) mExchange, epb, listener);
                    } else {
                        processInOnly((InOnly) mExchange, epb);
                    }
                } else if (pat.equals(MEP.ROBUST_IN_ONLY.toString())) {
                    OutboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00401.OMP_Not_supported_inonly",
                            new Object[]{mExchange.getExchangeId()}));
                } else if (pat.equals(MEP.OUT_IN.toString())) {
                    OutboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00402.OMP_Not_supported_outin",
                            new Object[]{mExchange.getExchangeId()}));
                } else if (pat.equals(MEP.OUT_ONLY.toString())) {
                    OutboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00403.OMP_Not_supported_outonly",
                            new Object[]{mExchange.getExchangeId()}));
                } else if (pat.equals(MEP.ROBUST_OUT_ONLY.toString())) {
                    OutboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00403.OMP_Not_supported_outonly",
                            new Object[]{mExchange.getExchangeId()}));
                } else {
                    OutboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00404.OMP_Invalid_pattern", new Object[]{exchangeId}));

                    return;
                }
            }
        } // if(exchange)

    }

    /**
     *
     * @param inonly
     * @param endpoint
     * @param listener
     */
    public void processInOnlyInbound(final InOnly inonly, final EndpointImpl endpoint,
            final MessageExchangeReplyListener listener) {
        OutboundMessageProcessor.mLogger.info(mMessages.getString("LDAPBC-R00417.OMP_Processing_InOnly_inbound"));

        if (inonly.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
        } else if (inonly.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
        } else {
            OutboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00407.OMP_Unexpected_ME_status",
                    new Object[]{inonly.getEndpoint(), inonly.getStatus()}));
        }

        try {
            listener.processReplyMessage(inonly);
        } catch (final Exception ex) {
            OutboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00406.OMP_Failed_processing_inonly_inbound",
                    new Object[]{ex}));
        }
    }

    /**
     *
     * @param inout
     * @param epb
     */
    public void processInOut(final InOut inout, final EndpointImpl epb) {
    	LdapConnection conn = null;
        if (inout.getStatus() == ExchangeStatus.DONE) {
            epb.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
            epb.getEndpointStatus().incrementReceivedErrors();
        } else {
            String faultCode = null;
            String faultDetail = null;
            boolean success = true;

            try {
				epb.getEndpointStatus().incrementReceivedRequests();
                Map operationNameToMetaData = (Map) epb.getValueObj(EndpointImpl.OPERATION_NAME_TO_META_DATA);
                OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inout.getOperation().getLocalPart());

                if (meta == null) {
                    throw new MessagingException(OutboundMessageProcessor.mMessages.getString(
                            "LDAPBC-R00418.OMP_oper_NotDefined") + inout.getOperation());
                }
                LDAPOperationAbstract ldapOperation;
                LDAPResponseAbstract result;

                final NormalizedMessage inMsg = inout.getInMessage();
                String mOperationtype = meta.getLDAPOperation().getOperationType();
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0214_Operation_Type", mOperationtype));

                if (mOperationtype.equals(LDAPOperation.OPERATION_TYPE_SEARCH) || mOperationtype.equals(LDAPOperation.OPERATION_TYPE_UPDATE) || mOperationtype.equals(LDAPOperation.OPERATION_TYPE_DELETE)) {

                    conn = search.getLdapConnection();
                    if (null == conn) {
                        conn = (LdapConnection) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
                    }
                } else if (mOperationtype.equals(LDAPOperation.OPERATION_TYPE_ADD)) {
                    	conn = add.getLdapConnection();
                    if (null == conn) {
                        conn = (LdapConnection) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
                    }
                }

                NormalizedMessage outMsg = mExchange.createMessage();
                String statusMessage = "";
                String jndiName = null;

                try {
                    LDAPDenormalizer denormalizer = new LDAPDenormalizer();
                    ldapOperation = denormalizer.denormalizeOutbound(inout.getInMessage(), meta, mExchange);
                } catch (Exception ex) {
                    success = false;
                    faultCode = CLIENT;
                    faultDetail = "Unable to denormalize message. The normalize message from consumer is not correct.";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0725.UnableToDenormalize", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    throw new Exception(faultString, ex);
                }
                try {
                    String opType = meta.getLDAPOperation().getOperationType();
                    result = processOperationResult(inout, ldapOperation, epb, meta, opType);
                } catch (PartialResultException ex) {
                    success = false;
                    faultCode = SERVER;
                    faultDetail = "Obtained partial result from the  external system";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0726.partialResultException", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    throw new Exception(faultString, ex);
                } catch (NameNotFoundException ex) {
                    success = false;
                    faultCode = SERVER;
                    faultDetail = "name not found in the LDAP  external system for deleting the entry specified";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0727.nameNotFound", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    throw new Exception(faultString, ex);
                } catch (NoPermissionException ex) {
                    success = false;
                    faultCode = SERVER;
                    faultDetail = "NO permission for the user  in the  LDAP  external system for deleting the entry specified";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0728.noPermission", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    throw new Exception(faultString, ex);
                } catch (javax.naming.CommunicationException ex) {
                    success = false;
                    faultCode = SERVER;
                    faultDetail = "Unable to connect to the  LDAP  external system ";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0728.noConnection", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    String errorString = "Unable to reconnect to the LDAP external system , Attempted to retry for " + mRtCfg.getRetryCount();
					String recoveryType = mRtCfg.getRecoveryType();
					if(recoveryType.equals("SUSPEND")){
						suspendEndpoint(epb);
						OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-I410.Suspend_Endpoint", epb.getEndpointName()));
					} else if(recoveryType.equals("ERROR")){
						OutboundMessageProcessor.mLogger.log(Level.INFO, errorString, ex.toString());
						throw new Exception(errorString, ex);
					} else if(recoveryType.equals("DELETE")){
						inout.setOutMessage(null);
						OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-I411.Message_Deleted", inout.getInMessage().toString()));
					}
                    throw new Exception(faultString.toString(), ex);
                }catch (NullPointerException ex) {
                    success = false;
                    faultCode = SERVER;
                    faultDetail = "Please provide a value for the attribute to be deleted/updated";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0729.noValueforUpdate", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    throw new Exception(faultString);
                }  catch (Exception ex) {
                    success = false;
                    faultCode = SERVER;
                    faultDetail = "Unable to connect to the  LDAP  external system ";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0728.noConnection", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    String errorString = "Unable to reconnect to the LDAP external system , Attempted to retry for " + mRtCfg.getRetryCount();
					String recoveryType = mRtCfg.getRecoveryType();
					if(recoveryType.equals("SUSPEND")){
						suspendEndpoint(epb);
						OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-I410.Suspend_Endpoint", epb.getEndpointName()));
					} else if(recoveryType.equals("ERROR")){
						OutboundMessageProcessor.mLogger.log(Level.INFO, errorString, ex.toString());
						throw new Exception(errorString, ex);
					} else if(recoveryType.equals("DELETE")){
						OutboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-I411.Message_Deleted", inout.getInMessage().toString()));
						inout.setOutMessage(null);
					}
                    throw new Exception(faultString.toString(), ex);
                }
                try {
                    LDAPNormalizer normalizer = new LDAPNormalizer();
                    outMsg = normalizer.normalize(result, inout, meta, mExchange);
                } catch (Exception ex) {
                    success = false;
                    faultCode = CLIENT;
                    faultDetail = "Unable to Normalize message recieved.";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0725.UnableToNormalize", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    throw new Exception(faultString, ex);


                }

                inout.setOutMessage(outMsg);

            } catch (Exception ex) {
                /*OutboundMessageProcessor.mLogger.log(Level.WARNING,
                OutboundMessageProcessor.mMessages.getString("LDAPBC-W00408.OMP_Failed_writing"), ex);
                statusMessage = "Failed " + ex.getMessage();
                success = false;
                inout.setError(ex);*/
                if (faultCode == null) {
                    success = false;
                    faultCode = SERVER;
                    faultDetail = "Unable to process message exchange.";
                    String faultString = mMessages.getString(
                            "LDAPBC-W0727.UnableToProcessMessageExchange", new Object[]{
                                inout.getExchangeId(), epb.toString()
                            });
                    ex = new Exception(faultString, ex);
                    
                }
                
                setErrorInExchange(inout, faultCode, faultDetail, ex);
            }finally{
//            	conn.closeConnection();
            }
            if (success) {
                epb.getEndpointStatus().incrementSentReplies();
            } else {
                epb.getEndpointStatus().incrementSentErrors();
            }
            try {
                mChannel.send(inout);

            } catch (Exception ex) {
                OutboundMessageProcessor.mLogger.log(Level.WARNING,
                        OutboundMessageProcessor.mMessages.getString("LDAPBC-E0749.DeliveryChannelSendFailed"), ex);
            }
        }
    }

    private void suspendEndpoint(EndpointImpl epb) {
    	try{
    	final ServiceEndpoint endpointReference = (ServiceEndpoint) epb.getValueObj(EndpointImpl.ENDPOINT_REFERENCE);
        mContext.deactivateEndpoint(endpointReference);
        }catch (final JBIException me) {
            me.printStackTrace();
        }
        epb.setValue(EndpointImpl.STATUS,EndpointImpl.STATUS_STOPPED);
	}

    /**
     *
     * @param inonly
     * @param epb
     */
    public void processInOnly(final InOnly inonly, final EndpointImpl epb) {
        try {
            epb.getEndpointStatus().incrementReceivedRequests();

            final NormalizedMessage inMsg = inonly.getInMessage();
            final Map operationNameToMetaData = (Map) epb.getValueObj(EndpointImpl.OPERATION_NAME_TO_META_DATA);
            final OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inonly.getOperation().getLocalPart());
            String jndiName = null;

            if (meta == null) {
                throw new MessagingException(OutboundMessageProcessor.mMessages.getString(
                        "LDAPBC-R00418.OMP_oper_NotDefined") + inonly.getOperation());
            }

            boolean success = true;

            try {
                inonly.setStatus(ExchangeStatus.DONE);
            } catch (final Exception ex) {
                success = false;
                OutboundMessageProcessor.mLogger.log(Level.WARNING,
                        OutboundMessageProcessor.mMessages.getString("LDAPBC-W00408.OMP_Failed_writing"), ex);
                inonly.setError(ex);
            }

            mChannel.send(inonly);

            if (success) {
                epb.getEndpointStatus().incrementSentDones();
            } else {
                epb.getEndpointStatus().incrementSentErrors();
            }
        } catch (final Exception ex) {
            OutboundMessageProcessor.mLogger.log(Level.WARNING,
                    OutboundMessageProcessor.mMessages.getString("LDAPBC-W00410.OMP_Failed_inonly"), ex);
        }
    }

    private LdapContext getConnection(EndpointImpl epb) {
        LdapContext ctx = null;
        try {
            Hashtable env = new Hashtable();
            ctx = (LdapContext) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return ctx;
    }

    /**
     * 
     * @param search
     * @return
     */
    private String getFilter(LDAPSearch search) {
        String ret = "";
        LDAPFilterGenerator filterGen = new LDAPFilterGenerator(search.getAttributes());
        try {
            ret += filterGen.generateFilter();
        } catch (MessagingException ex) {
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        }
        return ret;
    }

    private LDAPResponseAbstract processOperationResult(InOut inout, LDAPOperationAbstract ldapOperation, EndpointImpl epb, OperationMetaData meta, String opType) throws NamingException, Exception {
		if (OutboundMessageProcessor.mLogger.isLoggable(Level.FINEST)) {
			OutboundMessageProcessor.mLogger.log(Level.FINEST, mMessages.getString("LDAPBC_ACFG0213_LDAP_Processing_Accept_Message", 
												 new Object[]{ldapOperation.toString()}));
		}
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0233_Records_Per_Page", Integer.toString(meta.getRecordsPerPage())));

        if (opType.equals(LDAPOperation.OPERATION_TYPE_SEARCH)) {
            return ldapQuery(inout, (LDAPSearch) ldapOperation, epb, meta);
        } else if (opType.equals(LDAPOperation.OPERATION_TYPE_UPDATE)) {
            return ldapUpdate(inout, (LDAPUpdate) ldapOperation, epb, meta);
        } else if (opType.equals(LDAPOperation.OPERATION_TYPE_DELETE)) {
            return ldapDelete(inout, (LDAPDelete) ldapOperation, epb, meta);
        } else {
            return ldapAdd(inout, (LDAPAdd) ldapOperation, epb, meta);
        }
    }

    private LDAPDeleteResponse ldapDelete(InOut inout, LDAPDelete delete, EndpointImpl epb, OperationMetaData meta) throws NamingException, Exception {
        LDAPDeleteResponse res = new LDAPDeleteResponse();
        LDAPSearch searchDelete = delete.getLdapSearch();
        if (searchDelete == null) {
            return res;
        }
        LdapConnection connLDAP = searchDelete.getLdapConnection();
        if (null == connLDAP) {
            connLDAP = (LdapConnection) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
        }
        NormalizedMessage inMsg = inout.getInMessage();
		if (isDynamicEndpoint(inMsg)) {
			// fabricate address using values from NM props
			connLDAP = fabricateLdapAddress(inMsg, connLDAP);
		}        
        LdapContext ctx = null;
        ctx = connLDAP.getConnection(meta,mKeyStoreUtil);
        if (null == ctx) {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
            throw new Exception(OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
        }
        LdapName dn = new LdapName(searchDelete.getDN());
        SearchControls control = searchDelete.getSearchControls();
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0229_Size", Long.toString(control.getCountLimit())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0228_Scope", Integer.toString(control.getSearchScope())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0230_Timeout", Integer.toString(control.getTimeLimit())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0231_Deref_Link", Boolean.toString(control.getDerefLinkFlag())));		
        String filter = getFilter(searchDelete);

        try {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0204_LDAP_Process_Delete_Start"));
            NamingEnumeration results = ctx.search(dn, filter, control);
            if (results != null && !results.hasMore()) {
                OutboundMessageProcessor.mLogger.log(Level.SEVERE,
                        OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0202_RuntimeNotEntryToDelete"));
                res.setCode("32");
                res.setOpResult(false);
            }
            while (results != null && results.hasMore()) {
                SearchResult result = (SearchResult) results.next();
                DeleteEntry entry = new DeleteEntry(result.getNameInNamespace());
                entry.deleteEntry(ctx);
                res.setCode("0");
                res.setOpResult(true);
            }
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0205_LDAP_Process_Delete_Finish"));
        } catch (PartialResultException ex) {
            res.setCode("9");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NameNotFoundException ex) {
            res.setCode("32");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoPermissionException ex) {
            res.setCode("50");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        }

        return res;
    }

    private LDAPAddResponse ldapAdd(InOut inout, LDAPAdd add, EndpointImpl epb, OperationMetaData meta) throws NamingException, Exception {
        LDAPAddResponse res = new LDAPAddResponse();
        if (add == null) {
            return res;
        }
        LdapConnection connLDAP = add.getLdapConnection();
        if (null == connLDAP) {
            connLDAP = (LdapConnection) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
        }
        NormalizedMessage inMsg = inout.getInMessage();
		if (isDynamicEndpoint(inMsg)) {
			// fabricate address using values from NM props
			connLDAP = fabricateLdapAddress(inMsg, connLDAP);
		}         
        LdapContext ctx = null;
        ctx = connLDAP.getConnection(meta, mKeyStoreUtil);
        if (null == ctx) {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
//            return res;
            throw new Exception(OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
        }
        String newDN = add.getRDN() + "," + add.getDN();
        LdapName name = new LdapName(newDN);
        Attributes attrs = add.getAttributes();

        try {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0208_LDAP_Process_Add_Start"));
            DirContext sub = ctx.createSubcontext(name, attrs);
            res.setCode("0");
            res.setOpResult(true);
            res.setResponseDirContext(sub);
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0209_LDAP_Process_Add_Finish"));
        //conn.closeConnection();
        } catch (NameAlreadyBoundException ex) {
            res.setCode("68");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SchemaViolationException ex) {
            res.setCode("65");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvalidAttributeIdentifierException ex) {
            res.setCode("17");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoPermissionException ex) {
            res.setCode("50");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        }
        return res;
    }

    private LDAPResponse ldapQuery(InOut inout, LDAPSearch search, EndpointImpl epb, OperationMetaData meta) throws NamingException, Exception {
        LDAPResponse res = new LDAPResponse();
        if (search == null) {
            return res;
        }
        LdapConnection connLDAP = search.getLdapConnection();

        if (null == connLDAP) {
            connLDAP = (LdapConnection) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
        }
        NormalizedMessage inMsg = inout.getInMessage();
		if (isDynamicEndpoint(inMsg)) {
			// fabricate address using values from NM props
			connLDAP = fabricateLdapAddress(inMsg, connLDAP);
		}         
        LdapContext ctx = null;
        ctx = connLDAP.getConnection(meta, mKeyStoreUtil);

        if (null == ctx) {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
            throw new Exception(OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
        }

        LdapName dn = new LdapName(search.getDN());
        SearchControls control = search.getSearchControls();
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0229_Size", Long.toString(control.getCountLimit())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0228_Scope", Integer.toString(control.getSearchScope())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0230_Timeout", Integer.toString(control.getTimeLimit())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0231_Deref_Link", Boolean.toString(control.getDerefLinkFlag())));
        String filter = getFilter(search);
        int recordsPerPage = meta.getRecordsPerPage();
        String sortKey = meta.getSortByAttribute();
        String sortType = meta.getSortByType();
        SortControl sortControl = null;
        PagedResultsControl pagedResultsControl = null;

        if (sortKey != null && !"".equals(sortKey)) {
            try {
                boolean order = true;
                if ("DESC".equals(sortType)) {
                    order = false;
                }
                SortKey key = new SortKey(sortKey, order, null);
                sortControl = new SortControl(new SortKey[]{key}, Control.NONCRITICAL);
            } catch (IOException ex) {
                res.setCode("80");
                OutboundMessageProcessor.mLogger.log(Level.SEVERE, ex.getMessage());
            }
        }

        if (recordsPerPage != 0) {
            try {
                byte[] cookie = PagedResultInfo.getCookie(connLDAP.toString());
                if (cookie == null) {
                    pagedResultsControl = new PagedResultsControl(recordsPerPage, Control.CRITICAL);
                } else {
                    pagedResultsControl = new PagedResultsControl(recordsPerPage, cookie, Control.CRITICAL);
                }
            } catch (IOException ex) {
                res.setCode("80");
                OutboundMessageProcessor.mLogger.log(Level.SEVERE, ex.getMessage());
            }
        }

        List<Control> lcontrols = new ArrayList<Control>();
        if (sortControl != null) {
            lcontrols.add(sortControl);
        }

        if (pagedResultsControl != null) {
            lcontrols.add(pagedResultsControl);
        }

        if (lcontrols.size() != 0) {
            Control[] acontrols = new Control[lcontrols.size()];
            for (int i = 0; i < lcontrols.size(); i++) {
                acontrols[i] = lcontrols.get(i);
                ctx.setRequestControls(acontrols);
            }
        }

        try {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0206_LDAP_Process_Query_Start", new Object[]{mExchange.getExchangeId()}));
            NamingEnumeration results = ctx.search(dn, filter, control);
            res.setCode("0");

            while (results != null && results.hasMore()) {
                SearchResult result = (SearchResult) results.next();
                res.addEntry(result);
            }

            if (lcontrols.size() != 0) {
                Control[] controls = ctx.getResponseControls();
                if (controls != null) {
                    for (int i = 0; i < controls.length; i++) {
                        if (controls[i] instanceof PagedResultsResponseControl) {
                            PagedResultsResponseControl prrc =
                                    (PagedResultsResponseControl) controls[i];
                            //int total = prrc.getResultSize();
                            PagedResultInfo.setCookie(connLDAP.toString(), prrc.getCookie());
                            continue;
                        }
                        if (controls[i] instanceof SortResponseControl) {
                            SortResponseControl src =
                                    (SortResponseControl) controls[i];
                            if (!src.isSorted()) {
                                throw src.getException();
                            }
                        }
                    }
                }
            }
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0207_LDAP_Process_Query_Finish"));

        //conn.closeConnection();
        } catch (PartialResultException ex) {
            res.setCode("9");
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ReferralException ex) {
//            DirContext c = (DirContext) ex.getReferralContext();
            res.setCode("10");
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvalidSearchControlsException ex) {
            res.setCode("80");
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        }
        return res;
    }

    private LDAPUpdateResponse ldapUpdate(InOut inout, LDAPUpdate update, EndpointImpl epb, OperationMetaData meta) throws NamingException, Exception {
        LDAPUpdateResponse res = new LDAPUpdateResponse();
        LDAPSearch searchUpdate = update.getLdapSearch();
        if (searchUpdate == null) {
            return res;
        }
        LdapConnection connLDAP = searchUpdate.getLdapConnection();
        if (null == connLDAP) {
            connLDAP = (LdapConnection) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
        }
        NormalizedMessage inMsg = inout.getInMessage();
		if (isDynamicEndpoint(inMsg)) {
			// fabricate address using values from NM props
			connLDAP = fabricateLdapAddress(inMsg, connLDAP);
		}         
        LdapContext ctx = null;
        ctx = connLDAP.getConnection(meta, mKeyStoreUtil);
        if (null == ctx) {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
//            return res;
            throw new Exception(OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0212_LDAP_Process_No_Connection_Being_Established"));
        }
        LdapName dn = new LdapName(searchUpdate.getDN());
        SearchControls control = searchUpdate.getSearchControls();
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0229_Size", Long.toString(control.getCountLimit())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0228_Scope", Integer.toString(control.getSearchScope())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0230_Timeout", Integer.toString(control.getTimeLimit())));
		mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0231_Deref_Link", Boolean.toString(control.getDerefLinkFlag())));
        String filter = getFilter(searchUpdate);

        try {
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0210_LDAP_Process_Update_Start"));
            ModificationItem[] modItems = getUpdateAttributes(update);
            NamingEnumeration results = ctx.search(dn, filter, control);
            if (results != null && !results.hasMore()) {
                OutboundMessageProcessor.mLogger.log(Level.SEVERE,
                        OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0213_RuntimeNotEntryToUpdate"));
                res.setCode("32");
                res.setOpResult(false);
            }

            while (results.hasMore()) {
                SearchResult result = (SearchResult) results.next();
                ctx.modifyAttributes(result.getNameInNamespace(), modItems);
                res.setOpResult(true);
                res.setCode("0");
            }
//            OutboundMessageProcessor.mLogger.log(Level.SEVERE,
//                    OutboundMessageProcessor.mMessages.getString("LDAPBC_ACFG0211_LDAP_Process_Update_Finish"));
        //conn.closeConnection();
        } catch (PartialResultException ex) {
            res.setCode("9");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ReferralException ex) {
            DirContext c = (DirContext) ex.getReferralContext();
            res.setCode("10");
            res.setOpResult(false);
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NullPointerException npe){
        	throw npe;
        }
        return res;
    }

    private ModificationItem[] getUpdateAttributes(LDAPUpdate update) throws NamingException {
    	
    	int size = update.getUpdateAttributes().size();
        int i = 0;
        ModificationItem[] ret = new ModificationItem[size];
        Iterator it = update.getUpdateAttributes().iterator();
        while (it.hasNext()) {
            UpdateBean updatebean = (UpdateBean) it.next();
            String opType = updatebean.getOpType();
            if (opType.equals(UpdateBean.OPERATION_TYPE_ADD)) {
                UpdateBeanAdd addBean = (UpdateBeanAdd) updatebean;
                ret[i++] = new ModificationItem(DirContext.ADD_ATTRIBUTE,
                        new BasicAttribute(addBean.getAttrName(), addBean.getAddValue()));
                continue;
            }
            if (opType.equals(UpdateBean.OPERATION_TYPE_REMOVE)) {
                UpdateBeanRemove removeBean = (UpdateBeanRemove) updatebean;
                ret[i++] = new ModificationItem(DirContext.REMOVE_ATTRIBUTE,
                        new BasicAttribute(removeBean.getAttrName()));
                continue;
            }
            if (opType.equals(UpdateBean.OPERATION_TYPE_REMOVEALL)) {
                UpdateBeanRemoveAll removeAllBean = (UpdateBeanRemoveAll) updatebean;
                ret[i++] = new ModificationItem(DirContext.REMOVE_ATTRIBUTE,
                        new BasicAttribute(removeAllBean.getAttrName()));
                continue;
            }
            if (opType.equals(UpdateBean.OPERATION_TYPE_REPLACE)) {
                UpdateBeanReplace replaceBean = (UpdateBeanReplace) updatebean;
                ret[i++] = new ModificationItem(DirContext.REPLACE_ATTRIBUTE,
                        new BasicAttribute(replaceBean.getAttrName(), replaceBean.getNewValue()));
            }
        }
        return ret;
    	
   }

    private void setErrorInExchange(MessageExchange ex, String faultCode,
            String faultDetail, Exception e) {
        ex.setError(e);
        ex.setProperty("com.sun.jbi.crl.faultcode", faultCode);
        ex.setProperty("com.sun.jbi.crl.faultstring", e.getMessage());
        ex.setProperty("com.sun.jbi.crl.faultactor", "sun-ldap-binding");
        ex.setProperty("com.sun.jbi.crl.faultdetail", faultDetail);
    }

    /**
     * 
     * @param msg - normalized message whose NM properties wiil be extracted
     * @param curAddress - FTPAddress object which contains value that fabricated FTPAddress object
     * will fall back to if the NM property does not present
     * @return the FTPAddress object which contains binding info from NM properties of the message
     */
    private LdapConnection fabricateLdapAddress(NormalizedMessage msg, LdapConnection curLdapConn) {
        LdapConnection newLdapConn = new LdapConnection();
        Object nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_LOCATION);
        newLdapConn.setLocation(nmProp != null ? nmProp.toString() : curLdapConn.getLocation());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_PRINCIPAL);
        newLdapConn.setPrincipal(nmProp != null ? nmProp.toString() : curLdapConn.getPrincipal());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_CREDENTIAL);
        newLdapConn.setCredential(nmProp != null ? nmProp.toString() : curLdapConn.getCredential());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_SSLTYPE);
        newLdapConn.setSsltype(nmProp != null ? nmProp.toString() : curLdapConn.getSsltype());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_AUTHENTICATION);
        newLdapConn.setAuthentication(nmProp != null ? nmProp.toString() : curLdapConn.getAuthentication());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_PROTOCOL);
        newLdapConn.setProtocol(nmProp != null ? nmProp.toString() : curLdapConn.getProtocol());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_TRUSTSTORE_LOC);
        newLdapConn.setTruststore(nmProp != null ? nmProp.toString() : curLdapConn.getTruststore());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_TRUSTSTORE_PASSWORD);
        newLdapConn.setTruststorepassword(nmProp != null ? nmProp.toString() : curLdapConn.getTruststorepassword());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_TRUSTSTORE_TYPE);
        newLdapConn.setTruststoretype(nmProp != null ? nmProp.toString() : curLdapConn.getTruststoretype());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_KEYSTORE_LOC);
        newLdapConn.setKeystore(nmProp != null ? nmProp.toString() : curLdapConn.getKeystore());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_KEYSTORE_PASSWORD);
        newLdapConn.setKeystorepassword(nmProp != null ? nmProp.toString() : curLdapConn.getKeystorepassword());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_KEYSTORE_USERNAME);
        newLdapConn.setKeystoreusername(nmProp != null ? nmProp.toString() : curLdapConn.getKeystoreusername());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_KEYSTORE_TYPE);
        newLdapConn.setKeystoretype(nmProp != null ? nmProp.toString() : curLdapConn.getKeystoretype());
        nmProp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_TLS_SECURITY);
        newLdapConn.setTlssecurity(nmProp != null ? nmProp.toString() : curLdapConn.getTlssecurity());
        return newLdapConn;
    }

    private boolean isDynamicEndpoint(NormalizedMessage msg) {
        Object useDynEp = msg.getProperty(LDAPComponentContext.NM_PROP_LDAP_USE_DYN_EP_BINDING);
        if (useDynEp != null) {
            if (useDynEp.toString().equalsIgnoreCase("true") && mRtCfg.getAllowDynamicEndpoint().booleanValue()) {
                return true;
            }
        }
        return false;
    }
}
