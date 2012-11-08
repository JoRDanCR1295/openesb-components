package test.jbi.integration.testcases;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import javax.xml.namespace.QName;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.SAAssembler.Redelivery;
import test.jbi.integration.test.framework.SAAssembler.Throttling;
import test.jbi.integration.test.framework.impl.FileBCSUAssembler;

public class Helper {

	public static FileBCSUAssembler createTestSU(String name, String desc,
			String wsdlFileName, String inputDir, Class cls) throws IOException {
		FileBCSUAssembler su = new FileBCSUAssembler(name, desc);

		String wsdlPath = Configuration.getPath(cls, wsdlFileName);
		modifyWsdl(wsdlPath, inputDir);

		su.addWsdl(wsdlPath);
		su.setJbiFile(Configuration.getPath(cls, "jbi.xml"));

		return su;
	}

	public static String createTestSA(FileBCSUAssembler su, String saName,
			QName consumerService, String consumerEndpoint,
			QName providerService, String provideEndpoint, Redelivery rd,
			Throttling throt) throws IOException {

		SAAssembler sa = new SAAssembler(saName, saName);
		sa.addSUAssembler(su);
		// Now add connection
		sa.addConnection(consumerService, consumerEndpoint, providerService,
				provideEndpoint, rd, throt);

		return sa.assemble(Configuration.getWorkingDir());
	}

	public static void modifyWsdl(String wsdlPath, String inputDir) throws IOException {
		StringBuilder sb = getResourceContents(wsdlPath);

		String s = sb.toString().replace("FILE_WORKING_DIR",
				inputDir);

		s.toString();

		FileWriter writer = new FileWriter(wsdlPath);

		writer.write(s);

		writer.close();

	}

	public static StringBuilder getResourceContents(String fileResource) {

		StringBuilder sb = new StringBuilder();

		try {

			InputStream ins = new FileInputStream(fileResource);

			BufferedReader br = new BufferedReader(new InputStreamReader(ins));

			while (true) {
				String line = br.readLine();
				if (line == null)
					break;
				sb.append(line + "\n");

			}

			br.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return sb;
	}

	public static void writeInputFiles(String dir, int numFiles) throws IOException {

		for (int i = 0; i < numFiles; i++) {
			String fileName = "test.xml" + i;
			writeFile(dir,fileName, "hello world");
		}

	}

	public static void writeFile(String dir, String fileName, String content) throws IOException {
		
		File f = new File(dir);
		f.mkdirs();
	    FileWriter w = new FileWriter(dir + File.separator + fileName);
		w.write(content);
		w.close();

	}
}
