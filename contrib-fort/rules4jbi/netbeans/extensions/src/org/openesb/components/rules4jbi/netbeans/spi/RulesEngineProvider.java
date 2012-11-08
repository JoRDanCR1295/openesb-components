/*
 * @(#)RulesEngineProvider.java        $Revision: 1.1 $ $Date: 2008/12/17 23:16:38 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.spi;

/**
 * SPI interface allowing rules engine vendors to plug in their
 * JSR 94 related configuration values.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:16:38 $
 * 
 * @since 0.1
 */
public interface RulesEngineProvider {
    
    String getRuleServiceProviderURI();

    String getRuleServiceProviderClassName();
}
