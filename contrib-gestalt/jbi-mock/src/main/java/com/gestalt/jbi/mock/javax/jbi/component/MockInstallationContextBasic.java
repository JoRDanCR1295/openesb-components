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
 * MockInstallationContextBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.component;

import org.w3c.dom.DocumentFragment;

import javax.jbi.component.ComponentContext;
import java.util.Collections;
import java.util.List;


public class MockInstallationContextBasic implements MockInstallationContext {
    private boolean isInstall = false;

    public void setIsInstall(boolean isInstall) {
        this.isInstall = isInstall;
    }

    public String getComponentClassName() {
        return "";
    }

    public List getClassPathElements() {
        return Collections.EMPTY_LIST;
    }

    public String getComponentName() {
        return "";
    }

    public ComponentContext getContext() {
        return null;
    }

    public String getInstallRoot() {
        return "";
    }

    public DocumentFragment getInstallationDescriptorExtension() {
        return null;
    }

    public boolean isInstall() {
        return isInstall;
    }

    public void setClassPathElements(List list) {
    }
}
