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
 * @(#)SchedulerExtensionRegistry.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl;

import javax.xml.namespace.QName;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.*;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Port;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;

/**
 * Implements WSDL4J ExtensionRegistry for Scheduler.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerExtensionRegistry extends ExtensionRegistry {
    private static final long serialVersionUID = 5821543608915968580L;
    
    public SchedulerExtensionRegistry() {
        super();
        
        ExtensionDeserializer ed = new SchedulerExtensionDeserializer();
        ExtensionSerializer es = new SchedulerExtensionSerializer();
        
        initializeExtension(Binding.class, BindingEx.ELEM_TYPE,
                BindingExImpl.class, ed, es);
        initializeExtension(BindingOperation.class,
                BindingOperationEx.ELEM_TYPE, BindingOperationExImpl.class,
                ed, es);
        initializeExtension(BindingInput.class, TriggerEx.ELEM_TYPE,
                TriggerExImpl.class, ed, es);
        initializeExtension(Port.class, ActivePeriodEx.ELEM_TYPE,
                ActivePeriodExImpl.class, ed, es);
    }
    
    private void initializeExtension(Class parentType, QName elementType,
            Class extensionType, ExtensionDeserializer ed,
            ExtensionSerializer es) {
        mapExtensionTypes(parentType, elementType, extensionType);
        registerDeserializer(parentType, elementType, ed);
        registerSerializer(parentType, elementType, es);
    }
}
