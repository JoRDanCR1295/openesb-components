/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ParseXsds.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scriptsecore.process;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;


public class ParseXsds {

    public ParseXsds() {
    }

    /**
     * This method parses wsdls for retrieving the acceptAckXsd and
     * applicationAckXsd names.
     *
     * @throws Exception
     */
    public void parseForXsds(String serviceUnitRootPath)
            throws Exception {
        JAXBX2JCompilerHelper xjcHelper = new JAXBX2JCompilerHelper();
        List xsdList = listResourceFiles(new File(serviceUnitRootPath), "xsd");

        for (Iterator fileItr = xsdList.iterator(); fileItr.hasNext();) {
            File xsdFile = (File) fileItr.next();
            String str = xsdFile.getAbsolutePath();

            try {
                str = str.replace("\\", "/");
            } catch (Exception e) {
            }

            xjcHelper.setDestDir(serviceUnitRootPath);
            xjcHelper.parseSchema(str);
            xjcHelper.invokeXJC();

            //			 compile the generated JAXB Classes
            String classesDir = serviceUnitRootPath + File.separator +
                    ScriptSEConstants.CLASSES;
            File file = new File(classesDir);

            if (!file.exists()) {
                file.mkdir();
            }

            List sourceFiles = listResourceFiles(new File(
                    xjcHelper.getGeneratedSourceDir()), "java");
            String[] args = constructJavaCArgs(serviceUnitRootPath, sourceFiles);
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            JavaCompilerHelper.compile(args, baos);

            String jarFile = serviceUnitRootPath + File.separator +
                    ScriptSEConstants.ADDITIONAL_SC_JAR;
            JarUtil.compress(file, new File(jarFile));
            createPackageXML(serviceUnitRootPath);
        }
    }

    /**
     * List all xsd files in the currentDir and below
     */
    protected List listResourceFiles(File currentDir, String extension) {
        List cumulativeResults = new ArrayList();
        File[] filesInCurrentDir = currentDir.listFiles();

        for (int fileCount = 0; fileCount < filesInCurrentDir.length;
                fileCount++) {
            if (filesInCurrentDir[fileCount].isFile()) {
                if (filesInCurrentDir[fileCount].getName().toLowerCase().endsWith(extension)) {
                    cumulativeResults.add(filesInCurrentDir[fileCount]);
                }
            } else if (filesInCurrentDir[fileCount].isDirectory()) {
                List xsdsInSubDirectories = listResourceFiles(filesInCurrentDir[fileCount],
                        extension);
                cumulativeResults.addAll(xsdsInSubDirectories);
            }
        }

        return cumulativeResults;
    }

    /**
     * This method constructs the JavaC arguments as String[] and returns it
     *
     * @param sourceFiles - List soure files
     * @return args
     */
    private String[] constructJavaCArgs(String serviceUnitRootPath,
            List sourceFiles) {
        String[] args = null;

        if ((sourceFiles != null) && (sourceFiles.size() > 0)) {
            args = new String[4 + sourceFiles.size()];
            args[0] = "-classpath";
            args[1] = System.getProperty("java.class.path");
            args[2] = "-d";
            args[3] = serviceUnitRootPath + File.separator + "classes";

            int baseIndex = 4;

            for (int i = 0; i < sourceFiles.size(); ++i) {
                String path = (String) (((File) sourceFiles.get(i)).getAbsolutePath());
                args[baseIndex + i] = path;
            }
        }

        return args;
    }

    /**
     *
     * @param serviceUnitRootPath
     * @throws Exception
     */
    public void createPackageXML(String serviceUnitRootPath)
            throws Exception {
        JarFile jarFile = getJarFile(new File(serviceUnitRootPath));
        Enumeration eum = jarFile.entries();
        List pkgList = new ArrayList();
        String pkgName = null;

        while (eum.hasMoreElements()) {
            JarEntry entry = (JarEntry) eum.nextElement();

            if (entry.getName().endsWith("ObjectFactory.class")) {
                pkgName = entry.getName();
                pkgName = pkgName.substring(0, pkgName.lastIndexOf("/"));
                pkgName = pkgName.replaceAll("/", ".");

                if (!pkgList.contains(pkgName)) {
                    pkgList.add(pkgName);
                }
            }
        }

        // generate package info file.
        generatePakgInfo(serviceUnitRootPath + "/PackageInfo.xml", pkgList);
    }

    /**
     *
     * @param packageInfoFile
     * @param pakList
     * @throws Exception
     */
    private void generatePakgInfo(String packageInfoFile, List pakList)
            throws Exception {
        FileOutputStream fos = null;

        try {
            StringBuffer sb = new StringBuffer();
            sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            sb.append("\n");
            sb.append("  <packageInfo");
            sb.append(
                    " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">");

            for (int i = 0; i < pakList.size(); i++) {
                sb.append("\n");
                sb.append("   <file packageName=\"" + (String) pakList.get(i) +
                        "\" xsdName=\"value\">");
                sb.append("</file>");
            }

            sb.append("\n");
            sb.append(" </packageInfo>\n");

            String content = sb.toString();
            fos = new FileOutputStream(packageInfoFile);
            //FileUtil.copy(content.getBytes("UTF-8"), fos);
            store(content.getBytes("UTF-8"), fos);
        } catch (Exception e) {
            throw e;
        } finally {
            if (fos != null) {
                try {
                    fos.close();
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            }
        }
    }

    /**
     *
     * @param srcDir
     * @return
     * @throws IOException
     */
    private JarFile getJarFile(File srcDir) throws IOException {
        JarFile jar = null;

        if (srcDir != null) {
            String[] jarExt = new String[]{".jar"};
            List jarFiles = getFilesRecursively(srcDir, jarExt);
            File jarFile = (File) jarFiles.get(0);
            jar = new JarFile(jarFile);
        }

        return jar;
    }

    /**
     *
     * @param dir
     * @param filter
     * @return
     */
    public static List getFilesRecursively(File dir, FileFilter filter) {
        List ret = new ArrayList();

        if (!dir.isDirectory()) {
            return ret;
        }

        File[] fileNdirs = dir.listFiles(filter);

        for (int i = 0, I = fileNdirs.length; i < I; i++) {
            if (fileNdirs[i].isDirectory()) {
                ret.addAll(getFilesRecursively(fileNdirs[i], filter));
            } else {
                ret.add(fileNdirs[i]);
            }
        }

        return ret;
    }

    /**
     *
     * @param dir
     * @param extensions
     * @return
     */
    public static List getFilesRecursively(File dir, String[] extensions) {
        FileFilter filter = null;

        if (extensions[0].equals(".wsdl") || extensions[0].equals(".jar")) {
            filter = new ExtensionFilter(extensions);
        }

        return getFilesRecursively(dir, filter);
    }

    /**
     *
     * @param input
     * @param output
     * @throws IOException
     */
    private static void store(byte[] input, OutputStream output)
            throws IOException {
        ByteArrayInputStream in = new ByteArrayInputStream(input);
        byte[] buf = new byte[4096];

        for (int n = 0; (n = in.read(buf)) != -1;) {
            output.write(buf, 0, n);
        }

        output.flush();
    }
}
