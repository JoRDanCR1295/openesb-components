/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: DumpMessageExchange.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.util.Iterator;
import java.util.Set;
import javax.jbi.messaging.MessageExchange;

public class DumpMessageExchange {
	
	public static String dump(MessageExchange me) {
		String m = "";
		m=m.concat("\nMessageExchange: "+me+"\n"+
					"getEndpoint="+me.getEndpoint()+"\n"+
					"endpoint service="+me.getEndpoint().getServiceName()+"\n"+
					"endpoint name="+me.getEndpoint().getEndpointName()+"\n"+
					"getError="+me.getError()+"\n"+
					"getExchangeId="+me.getExchangeId()+"\n"+
					"getFault="+me.getFault()+"\n"+
					"getInterfaceName="+me.getInterfaceName()+"\n"+
					"getMessage="+me.getMessage("in")+"\n"+
					"getOperation="+me.getOperation()+"\n"+
					"getPattern="+me.getPattern()+"\n");
					
		Set propNames = me.getPropertyNames();
		for (Iterator i = propNames.iterator(); i.hasNext();) {
			String name = (String)i.next();
			m=m.concat("property("+name+")="+me.getProperty(name)+"\n");
		}
		
		m=m.concat("getRole="+me.getRole()+"\n"+
					"getService="+me.getService()+"\n"+
					"getStatus="+me.getStatus()+"\n"+
					"isTransacted="+me.isTransacted()+"\n");
		return m;
	}
}
