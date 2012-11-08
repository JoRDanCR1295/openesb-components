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
 * $Id: CcslUpoc.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.script.Invocable;
import javax.script.ScriptEngine;

import org.apache.commons.logging.Log;

public class CcslUpoc {

	public static Object runScript(Log log, String rootDir, String upocContext, String upocType, String upocClass, String upocMethod, 
			ComponentContext componentContext, DeliveryChannel channel, MessageExchange exchange, Map<String, String> params, Object parent ) throws Exception {
		LinkedList sendList = new LinkedList();
		
		
		// run the script
		Object[] scriptArgs = {log, upocContext, componentContext, channel, exchange, params};
		Object rtn = runScript(rootDir, upocType, upocClass, upocMethod, scriptArgs,parent);

		if (rtn instanceof LinkedList) sendList = (LinkedList)rtn;
		return sendList;
	}
	
	public static Object runScript(Log log, String rootDir,  String upocType, String upocClass, String upocMethod, ComponentContext componentContext, 
			DeliveryChannel channel, MessageExchange exchange, Map<String, String> params) throws Exception {
		LinkedList sendList = new LinkedList();
	
		// run the script
		Object[] scriptArgs = {log, componentContext, channel, exchange, params};
		Object rtn = runScript(rootDir, upocType, upocClass, upocMethod, scriptArgs);
		

		if (rtn instanceof LinkedList) sendList = (LinkedList)rtn;
		return sendList;
	}
	
	public static Object runScript(Log log, String rootDir,  String upocType, String upocClass, String upocMethod, ComponentContext componentContext, 
			DeliveryChannel channel, Map<String, String> params) throws Exception {
		LinkedList sendList = new LinkedList();
		
		// run the script
		Object[] scriptArgs = {log, componentContext, channel, params};
		Object rtn = runScript(rootDir, upocType, upocClass, upocMethod, scriptArgs);

		
		if (rtn instanceof LinkedList) sendList = (LinkedList)rtn;
		return sendList;
	}
	
	public static Object runScript( String rootDir,  String upocType, String upocClass, String upocMethod, Object[] scriptArgs ) throws Exception {
		return runScript(rootDir, upocType, upocClass, upocMethod, scriptArgs, null );
	}
	
	public static Object runScript( String rootDir,  String upocType, String upocClass, String upocMethod, Object[] scriptArgs, 
			Object parent ) throws Exception {
		// get the UpocInstance
		UpocInstance inst = UpocInstance.getUpocInstance(upocType, upocClass, rootDir, parent);
		 if(upocType.equalsIgnoreCase("Groovy")){
				ScriptEngine se = inst.getEngine();
						
		// run the script

				Object rtn = ((Invocable)se).call( upocMethod, scriptArgs);
				return rtn;
		 }
		 else if(upocType.equalsIgnoreCase("Pojo") || upocType.equalsIgnoreCase("Java") ){
			 Class pojoClass=inst.getPojoClass();
			 Method[] methods=pojoClass.getDeclaredMethods();
			 for(int i=0;i<methods.length;i++){
				 if(methods[i].getName().equals(upocMethod)){
					 
					 Object rtn= methods[i].invoke(inst.getPojoClassInstance(), scriptArgs);
					 return rtn;
				 }
			 }
		 }
		 return null;
	}
}
