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
 * @(#)EmailBCEndpointManager.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email;

import com.sun.jbi.binding.email.protocol.EmailBindingComponentConfigurationException;
import java.util.Map;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;

import com.sun.jbi.binding.email.protocol.EmailWSDLConfigurations;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import com.sun.jbi.binding.email.protocol.send.smtp.SMTPEndpoint;
import javax.wsdl.WSDLException;

/**
 * Manages Email Binding Component endpoints and acts as 
 * its own {@link Endpoint} factory.
 * 
 * @author CDK
 */
public class EmailBCEndpointManager extends AbstractEndpointManager {

    /** Constructs a Email Binding Component endpoint manager. */
    public EmailBCEndpointManager(ComponentContext ctx) {
        super(ctx);	// this EndpointManager acts as its own EndpointFactory
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#createEndpoint(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.descriptor.ServiceUnit) */
    public Endpoint<Object> createEndpoint(EndpointInfo info, ServiceUnit srvcUnit)
            throws DeploymentException {
        // The following line switches classloaders to load application jars deployed with a service unit
        getContext().getCustomClassLoaderUtil().switchClassLoader(srvcUnit.getName(), SwitchType.service_classloader);
        try {
            // TODO (required) Create an EmailBCEndpoint instance
            EmailWSDLConfigurations wsdlConfig = new EmailWSDLConfigurations(this.getContext(), info, srvcUnit.getRootPath());
            EmailBCEndpoint endpt = wsdlConfig.createEmailEndpoint();
            if(endpt instanceof SMTPEndpoint) {
	            Map encoderMapping = wsdlConfig.getPartEncoderMapping(
	            		endpt.getInfo().getServiceName().toString(),
	            		endpt.getInfo().getEndpointName(),
	            		endpt.getEmailOperations());
	            endpt.setMessagePartEncoderMapping(encoderMapping);
            }
            return endpt;
        } catch (WSDLException e) {
            throw new DeploymentException(I18n.loc("EMAILBC-7013: Failed to create endpoint from EndpointInfo: {0}, and ServiceUnit: {1}", info, srvcUnit), e);
        } catch (EmailBindingComponentConfigurationException e) {
            throw new DeploymentException(I18n.loc("EMAILBC-7013: Failed to create endpoint from EndpointInfo: {0}, and ServiceUnit: {1}", info, srvcUnit), e);
        } catch(Exception e){
        	throw new DeploymentException(I18n.loc("EMAILBC-7036: Failed to create Encoder Mapping for the EndpointInfo: {0}, and ServiceUnit: {1}", info, srvcUnit), e);
        } finally {
            // switch back to original classloader...
            getContext().getCustomClassLoaderUtil().switchClassLoader(srvcUnit.getName(), SwitchType.context_classloader);
        }
    }
}

