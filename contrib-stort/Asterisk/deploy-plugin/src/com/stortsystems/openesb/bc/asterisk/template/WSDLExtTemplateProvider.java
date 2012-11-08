/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: WSDLExtTemplateProvider.java,v 1.1 2008/01/20 16:40:09 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.template;

import java.io.InputStream;
import org.netbeans.modules.xml.wsdl.bindingsupport.spi.ExtensibilityElementTemplateProvider;
import org.openide.util.NbBundle;

public class WSDLExtTemplateProvider extends ExtensibilityElementTemplateProvider {
    
    private final String templateUrl = "template.xml"; // relative path.
    
    public InputStream getTemplateInputStream() {
        return this.getClass().getResourceAsStream(templateUrl);
    }
    
    public String getLocalizedMessage(String str, Object[] objects) {
        return NbBundle.getMessage(this.getClass(), str, objects);
    }
}
