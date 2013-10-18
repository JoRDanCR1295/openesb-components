package test.jbi.integration.test.framework.impl;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import test.jbi.integration.test.framework.SUAssembler;
import test.jbi.integration.testbc.installer.TestHelper;

public class FileBCSUAssembler implements SUAssembler {

    private String name;
    private String desc;
    private List<String> wsdls;
    private String jbiFile;
    private static final String XML = "<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n";

    public FileBCSUAssembler(String name, String desc) {
        this.name = name.replace(' ', '_');
        this.desc = desc;
        wsdls = new ArrayList<String>();
    }

    public void addWsdl(String wsdl) {
        wsdls.add(wsdl);
    }

    public boolean removeWsdl(String wsdl) {
        return wsdls.remove(wsdl);
    }

    public String getJbiFile() {
        return jbiFile;
    }

    public void setJbiFile(String jbiFile) {
        this.jbiFile = jbiFile;
    }

    public String assemble(String workingDir, String destDir) throws IOException {
        String base = workingDir + File.separator + name;
        //Create base directory
        File baseDir = new File(base);
        if (baseDir.exists()) {
            TestHelper.deleteDirectory(baseDir);
        }
        baseDir.mkdirs();

        String meta = base + File.separator + "META-INF";
        File metaDir = new File(meta);
        metaDir.mkdir();

        //Copy all WSDL files to META-INF folder
        for (Iterator<String> iter = wsdls.iterator(); iter.hasNext();) {
            TestHelper.copyFile(meta, iter.next());
        }

        //copy jbi.xml into META-INF
        TestHelper.copyFile(meta, getJbiFile(), "jbi.xml");

        String jarFileName = name + ".jar";
        String destJar = destDir + File.separator + jarFileName;
        TestHelper.jarAllFiles(base, destJar);
        return jarFileName;
    }

    public String getComponentName() {
        return "sun-file-binding";
    }

    public String getDescription() {
        return desc;
    }

    public String getName() {
        return name;
    }
}
