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
 * MockBootstrapBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.component;

import javax.jbi.JBIException;
import javax.jbi.component.InstallationContext;

import javax.management.ObjectName;


public class MockBootstrapBasic implements MockBootstrap {
    public void init(InstallationContext installationContext)
        throws JBIException {
    }

    public void cleanUp() throws JBIException {
    }

    public ObjectName getExtensionMBeanName() {
        return null;
    }

    public void onInstall() throws JBIException {
    }

    public void onUninstall() throws JBIException {
    }
}
