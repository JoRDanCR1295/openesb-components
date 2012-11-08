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
 * @(#)BindingOperationEx.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain.wsdl4jext;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants;

/**
 * Describes the Scheduler WSDL Operation extension.
 * @author sunsoabi_edwong
 */
public interface BindingOperationEx extends ExtensibilityElement,
        SchedulerConstants {

    public static final QName ELEM_TYPE = new QName(SCHEDULER_SCHEMA_NAMESPACE,
            BINDINGOPERATIONEX_ELEM_NCNAME, SCHEDULER_PREFIX);
    
    String getMode();
    
    void setMode(String mode);
    
}
