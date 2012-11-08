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
 * $Id: UpocInstance.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.HashMap;

import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Namespace;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.bostechcorp.cbesb.common.util.RuntimeClassLoader;

public class UpocInstance {
	private static HashMap<UpocInstanceKey, UpocInstance> upocInstanceMap = new HashMap<UpocInstanceKey, UpocInstance>();
	private static ScriptEngineManager manager = new ScriptEngineManager();
	
	private CompiledScript thisCompiled;
	private Namespace thisNamespace;
	private ScriptEngine thisEngine;
	private Class thisClass;
	private Object thisClassInstance;
	
	public UpocInstance(CompiledScript compiled, Namespace namespace, ScriptEngine engine,Class pojoClass, Object pojoClassInstance) {
		thisCompiled = compiled;
		thisNamespace = namespace;
		thisEngine = engine;
		thisClass=pojoClass;
		thisClassInstance = pojoClassInstance;
	}
	
	public static synchronized UpocInstance getUpocInstance(String upocType, String upocClass, String upocRootDir) throws ScriptException, FileNotFoundException,ClassNotFoundException, 
			InstantiationException, IllegalAccessException {
		return getUpocInstance(upocType, upocClass, upocRootDir, null);
	}
	
	public static synchronized UpocInstance getUpocInstance(String upocType, String upocClass, String upocRootDir, Object parent) throws ScriptException, FileNotFoundException,
			ClassNotFoundException, InstantiationException, IllegalAccessException {
		UpocInstanceKey thisKey = new UpocInstanceKey(upocType, upocClass, upocRootDir);
		UpocInstance thisInstance = (UpocInstance)upocInstanceMap.get(thisKey);
		if (thisInstance == null) {
			// instantiate a new UpocInstance
			  if(upocType.equalsIgnoreCase("Groovy")){
				ScriptEngine se = manager.getEngineByName(upocType);
				CompiledScript compiled = (((Compilable)se).compile(new FileReader(new File(upocRootDir, upocClass))));
				Namespace namespace = se.createNamespace();
				compiled.eval(namespace);
				thisInstance = new UpocInstance(compiled, namespace, se,null, null);
				upocInstanceMap.put(thisKey, thisInstance);
			  }
			  else if(upocType.equalsIgnoreCase("Pojo") || upocType.equalsIgnoreCase("Java")){
				  String saName = CcslUtil.getAssemblyNameFromWorkspaceDir(upocRootDir);
				  Class pojoClass = Class.forName(upocClass, true, RuntimeClassLoader.getClassLoader(saName, parent));
				  thisInstance = new UpocInstance(null, null, null,pojoClass, pojoClass.newInstance());
				  upocInstanceMap.put(thisKey, thisInstance);

			  }
		}
		return thisInstance;
	}
	
	public ScriptEngine getEngine() {
		return thisEngine;
	}
	
	public Namespace getNamespace() {
		return thisNamespace;
	}
	
	public CompiledScript getCompiledScript() {
		return thisCompiled;
	}

	public Class getPojoClass() {
		return thisClass;
	}

	public Object getPojoClassInstance() {
		return thisClassInstance;
	}

	
	
}
