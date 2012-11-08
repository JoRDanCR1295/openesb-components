 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.wsdl;


import java.util.Map;

import com.ibm.wsdl.extensions.PopulatedExtensionRegistry;

/**
 * Jbi4CorbaExtensionRegistry.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaExtensionRegistry extends PopulatedExtensionRegistry {
    private static final long serialVersionUID = 1L;

    /** Creates a new instance of Jbi4CorbaExtensionRegistry */
   public Jbi4CorbaExtensionRegistry(Jbi4CorbaAddressDeserializer serializer) {
       super();
       Jbi4CorbaExtension.register(this);
       Jbi4CorbaExtension.overrideAddressDeserializer(this, serializer);
       
    }

    /** Creates a new instance of Jbi4CorbaExtensionRegistry */
     public Jbi4CorbaExtensionRegistry(Map<String, String[]> envVarConfigMap) {
        super();
        Jbi4CorbaAddressDeserializer jbiExtSerializer = new Jbi4CorbaAddressDeserializer(envVarConfigMap);
        Jbi4CorbaExtension.register(this);
		Jbi4CorbaExtension.overrideAddressDeserializer(this, jbiExtSerializer);
    }     

}
