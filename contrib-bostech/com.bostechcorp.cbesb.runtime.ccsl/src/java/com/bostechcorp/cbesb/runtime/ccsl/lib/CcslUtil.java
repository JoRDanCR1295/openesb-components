package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.File;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.bostechcorp.cbesb.common.util.Dom;
import com.bostechcorp.cbesb.common.util.EsbPathHelper;

public class CcslUtil {

	
	/*
	 * TODO - FIXME
	 * the wdir files are container specific. Looking through the directory structure is not JBI compliant 
	 * and must be changed.
	 */
	
	public static String getAssemblyNameFromWorkspaceDir(String workspaceDir) {
		String assemblyName = null;
		int start = workspaceDir.indexOf("ideworkspace");
		if (start >= 0) {
			start += 13;
			int end = workspaceDir.indexOf("/", start);
			if (end < 0) end = workspaceDir.indexOf("\\", start);
			if (end >= 0) assemblyName = workspaceDir.substring(start, end);
			else assemblyName = workspaceDir.substring(start);
		}
		return assemblyName;
	}
	
	
	public static String getAssemblyName(String suRootPath) throws Exception{
	//	System.out.println("rootPath:"+suRootPath);
		Document doc=Dom.getDomTree(new File(suRootPath+"/../../../install/META-INF/jbi.xml"),null);
		
		Node sa=Dom.findChild(doc.getDocumentElement(), "service-assembly", true);
		Node saId=Dom.findChild(sa, "identification",false);
		Node saName=Dom.findChild(saId, "name", false);
		String name=saName.getTextContent();
		return name;
	}
	
	public static String getSaProjectPath(String suRootPath) throws Exception{
		String ws = EsbPathHelper.getCbesbUiWorkSpace();
		return ws+"/"+getAssemblyName(suRootPath);
	}
	
	public static String getScriptPathPath(String suRootPath) throws Exception{
		return getSaProjectPath(suRootPath)+"/src/script";
	}
	
}
