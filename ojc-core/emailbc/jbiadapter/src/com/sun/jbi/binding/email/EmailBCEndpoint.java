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
 * @(#)EmailBCEndpoint.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email;

import java.util.Map;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.sun.jbi.binding.email.protocol.receive.InboundConsumer;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.config.AppConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpoint;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import java.util.HashMap;
import java.util.logging.Logger;

/**
 * The {@link com.sun.jbi.component.toolkit.endpoint.Endpoint Endpoint} 
 * implementation for Email Binding Component.
 * 
 * @author CDK
 */
//public class EmailBCEndpoint extends AbstractEndpoint<Object> {
public abstract class EmailBCEndpoint extends AbstractEndpoint<Object> {
    private static final Logger logger = Logger.getLogger(EmailBCEndpoint.class.getName());
    private Definition mDef = null;
    //Map of <operationName, EmailOperation>
    private Map<QName, ExtensibilityElement> mBindingOperations = null;
    private Map mPartEncoderMap = null;
    private ManagerContext mMgrCtx;
    private AppConfig mAppConfig;
    private ThrottlingConfig mThrottlingConfig;
    // internal variables
    private InboundConsumer mConsumer;
    private Map<String, String> nmProperties = new HashMap<String, String>();

    /**
     * Construct a Email Binding Component endpoint.
     * @param ctx The context for the ComponentManager.
     * @param info The JBI descriptor entry describing this endpoint.
     * @param suRootPath Service Unit Root Path
     * @throws DeploymentException 
     */
    public EmailBCEndpoint(ManagerContext ctx, EndpointInfo info, AppConfig appConfig) {
        super(info);
        this.mMgrCtx = ctx;
        mAppConfig = appConfig;
        populateNMProperties();
        warnUnsupportedFeatures();
    }

    /**
     * @return
     */
    public ManagerContext getContext() {
        return this.mMgrCtx;
    }

    public Definition getDefinition() {
        return this.mDef;
    }

    public void setDefinition(Definition def) {
        this.mDef = def;
    }

    public Map getMessagePartEncoderMapping() {
        return this.mPartEncoderMap;
    }

    public void setMessagePartEncoderMapping(Map map) {
        this.mPartEncoderMap = map;
    }

    public Map<QName, ExtensibilityElement> getEmailOperations() {
        return this.mBindingOperations;
    }

    public void setEmailOperations(Map<QName, ExtensibilityElement> operations) {
        this.mBindingOperations = operations;
    }

    public AppConfig getApplicationConfiguration() {
        return this.mAppConfig;
    }

    public ThrottlingConfig getThrottlingConfig() {
        if (null == this.mThrottlingConfig) {
            this.mThrottlingConfig = this.mMgrCtx.getMessagingChannel().getServiceQuality(this.getInfo(), ThrottlingConfig.class);
        }
        return this.mThrottlingConfig;
    }

    /**
     * @see com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpoint#start()
     */
    @Override
    public void start() throws JBIException {
        if (!this.getInfo().isProvides()) {
            this.mConsumer = new InboundConsumer(this);
            this.mConsumer.startConsuming();
        }
        super.start();
    }

    /**
     * @see com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpoint#stop()
     */
    @Override
    public void stop() throws JBIException {
        if (!this.getInfo().isProvides()) {
            if (null != this.mConsumer) {
                this.mConsumer.stopConsuming();
            }
        }
        super.stop();
    }

    private void warnUnsupportedFeatures() {
        //Warn about Redelivery 'suspend' option not supported
        RedeliveryConfig rConfig = this.mMgrCtx.getMessagingChannel().getServiceQuality(this.getInfo(), RedeliveryConfig.class);
        if (rConfig != null) {
            if (rConfig.getFailure() == RedeliveryConfig.Failure.suspend) {
                I18n.warning(logger, "EMAILBC-6014: Redelivery option \'suspend\' is not supported, if enabled, this will be handled like \'error\' option");
            }
        }
    }

    public Map<String, String> getNMProperties() {
        return nmProperties;
    }

    private void populateNMProperties() {
        
    }


}

