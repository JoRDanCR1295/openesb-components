package test.jbi.integration.testbc.installer;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class TestBCInstaller {
	
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

		String beginsWith = TestBCInstaller.class.getName();
		beginsWith = beginsWith.substring(0, beginsWith.lastIndexOf('.'));
		beginsWith = beginsWith.substring(0, beginsWith.lastIndexOf('.')).replace('.', '/');
		//beginsWith = "classes/" + beginsWith + "/";  
		beginsWith = beginsWith + "/";  

		URL url = TestBCInstaller.class.getResource("TestBCInstaller.class");
		String path = url.getPath();
		path = path.substring(0, path.lastIndexOf('/'));
		path = path.substring(0, path.lastIndexOf('/'));

		List<String[]> list = new ArrayList<String[]>();
		TestHelper.recursiveTraverse(new File(path), list, beginsWith);
		List<String[]> baseList = new ArrayList<String[]>(3);
		baseList.add(new String[]{destDir + "/testbcimpl.jar", "lib/"});
		int breakCount = 0;
		for(Iterator<String[]> iter=list.iterator(); iter.hasNext() && breakCount < 2;){
			String[] arr = iter.next();
			if(arr[0].endsWith("jbi.xml")){
				arr[1] = "META-INF/";
				iter.remove();
				baseList.add(arr);
				breakCount++;
			} else if(arr[0].endsWith("MANIFEST.MF")){
				arr[1] = "META-INF/";
				iter.remove();
				baseList.add(arr);
				breakCount++;
			}
		}
		String jarName  = destDir + "/testbcimpl.jar";
		TestHelper.jarFiles(jarName, list);
		
		jarName  = destDir + "/TestBC.jar";
		TestHelper.jarFiles(jarName, baseList);
		
		sInstaller = jarName;
		return jarName;
		
	}
	
}
