/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskWSDLProcessor.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.stortsystems.openesb.bc.asterisk.wsdlext.WSDLExtensionRegistry;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import java.util.*;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;

import java.io.*;

public class AsteriskWSDLProcessor extends WSDLProcessor {
    private WSDLExtensionRegistry mExtRegistry;
    /** Creates a new instance of AsteriskWSDLProcessor */
    public AsteriskWSDLProcessor(String wsdlDir) {
        super(wsdlDir);
    }

   
    @Override
    protected ExtensionRegistry getExtensionRegistry() {
        if ( this.mExtRegistry == null ) {
            this.mExtRegistry = new WSDLExtensionRegistry();
        }
        return this.mExtRegistry;
    }
}
