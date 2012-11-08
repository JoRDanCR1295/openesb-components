/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: WSDLExtValidatorSchemaFactory.java,v 1.1 2008/01/20 16:40:08 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.validator;

import java.io.InputStream;
import java.io.InputStream;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import org.netbeans.modules.xml.wsdl.validator.spi.ValidatorSchemaFactory;

public class WSDLExtValidatorSchemaFactory extends ValidatorSchemaFactory {
    
    private final String NS_URL =
            "http://java.sun.com/jbi/wsdl-extensions/sample/asterisk-bc/";
    private final String wsdlExtXSDResourcePath =
            "/com/stortsystems/openesb/bc/asterisk/AsteriskWsdlExt.xsd";;
            
            public String getNamespaceURI() {
                return NS_URL;
            }
            
            public InputStream getSchemaInputStream() {
                return this.getClass().getResourceAsStream(wsdlExtXSDResourcePath);
            }
            /**
             * Returns the Inputstream related to this schema
             */
            public Source getSchemaSource() {
                InputStream in = this.getClass().getResourceAsStream(wsdlExtXSDResourcePath);
                Source s = new StreamSource(in);
                s.setSystemId(this.getClass().getResource(wsdlExtXSDResourcePath).toString());
                return s;
            }
            
}
