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
 * @(#)$Id: WLMSEBootStrap.java,v 1.1 2010/02/15 19:25:07 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.component.toolkit.lifecycle.AbstractBootstrap;
import com.sun.jbi.engine.workflow.util.I18n;

public class WLMSEBootStrap extends AbstractBootstrap {

    @Override
    protected Object initInstallMBean(ComponentContext ctx) throws JBIException {
        // TODO Auto-generated method stub
        try {
            // pass config to avoid duplicate initialization from descriptor by AcceptConfig
            return new WLMSEConfiguration(ctx, getConfig());
        }
        catch (Exception e) {
            throw new JBIException(I18n.loc("WLM-6002: Caught exception while creating Installatoin Configuration MBean, failed to init workflowse component"), e);
        }

    }

}
