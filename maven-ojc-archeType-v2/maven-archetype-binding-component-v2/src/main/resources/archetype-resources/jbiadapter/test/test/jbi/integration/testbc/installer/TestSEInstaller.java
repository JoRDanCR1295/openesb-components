#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package test.jbi.integration.testbc.installer;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class TestSEInstaller {
	
	private static String sInstaller;
	
	synchronized public static String generateInstaller(String destDir) throws IOException{
	
		if(sInstaller != null)
			return sInstaller;
		
		//Clear the directory
		File f = new File(destDir);
		if(f.exists()){
			TestHelper.deleteDirectory(f);
		}
		f.mkdirs();

		String beginsWith = TestSEInstaller.class.getName();
		beginsWith = beginsWith.substring(0, beginsWith.lastIndexOf('.'));
		beginsWith = beginsWith.substring(0, beginsWith.lastIndexOf('.')).replace('.', '/');
		beginsWith = "classes/" + beginsWith + "/";  

		URL url = TestSEInstaller.class.getResource("TestSEInstaller.class");
		String path = url.getPath();
		path = path.substring(0, path.lastIndexOf('/'));
		path = path.substring(0, path.lastIndexOf('/'));

		List<String[]> list = new ArrayList<String[]>();
		TestHelper.recursiveTraverse(new File(path), list, beginsWith);
		for(Iterator<String[]> iter=list.iterator(); iter.hasNext();){
			String[] arr = iter.next();
			if(arr[0].endsWith("jbi.xml")){
				arr[1] = "META-INF/";
				break;
			}
		}
		
		String jarName  = destDir + "/TestSE.jar";
		TestHelper.jarFiles(jarName, list);
		sInstaller = jarName;
		return jarName;
		
	}
	
}
