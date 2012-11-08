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
 * @(#)FtpFileProviderTester.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.ftp;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Properties;
import java.util.StringTokenizer;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;

/*
 *
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class FtpFileProviderTester {
    
    public FtpFileProviderTester() {
    }
    
    public static void main(String args[])
    throws Exception {
        String dirListingStyle;
        String host;
        FtpFileProviderImpl ftp;
        BufferedReader reader;
        String inputLine;
        System.out.println("\n\n\n");
        System.out.println("********************************************");
        System.out.println("*                Start                     *");
        System.out.println("********************************************");
        dirListingStyle = "UNIX";
        host = "localhost";
        int port = 21;
        String userid = "anonymous";
        String password = "harry";
        ftp = null;
        reader = new BufferedReader(new InputStreamReader(System.in));
        System.out.println("Host (" + host+ "): ");
        inputLine = reader.readLine();
        if(inputLine != null && !inputLine.equals("")) {
            host = inputLine;
        }
        System.out.println("Port ("+ port+ "): ");
        inputLine = reader.readLine();
        if(inputLine != null && !inputLine.equals("")) {
            port = Integer.parseInt(inputLine);
        }
        System.out.println("DirListingStyle ("+ dirListingStyle + "): ");
        inputLine = reader.readLine();
        if(inputLine != null && !inputLine.equals("")) {
            dirListingStyle = inputLine;
        }
        try {
            boolean old = false;
            if(old) {
                ftp = new FtpFileProviderImpl();
            } else {
                BatchFtp etd = new BatchFtp();
                etd.initialize(new Properties());
                ftp = (FtpFileProviderImpl)etd.getProvider();
            }
            ftp.setDirListingStyle(dirListingStyle);
        } catch(Exception e) {
            System.out.println("Could not create ftp handle.");
            e.printStackTrace();
            return;
        }
        try {
            ftp.connect(host, port);
            System.out.println(ftp.getReplyString());
            int reply = ftp.getReplyCode();
            if(!ftp.isPositiveCompletion(reply)) {
                ftp.disConnect();
                System.out.println(ftp.getReplyString());
                return;
            }
        } catch(Exception e) {
            System.out.println("Could not connect to server.");
            if(ftp.isConnected()) {
                try {
                    ftp.disConnect();
                    System.out.println(ftp.getReplyString());
                } catch(IOException ie) { }
            }
            e.printStackTrace();
            return;
        }
        try {
            System.out.println("User (" + userid + "): ");
            inputLine = reader.readLine();
            if(inputLine != null && !inputLine.equals("")) {
                userid = inputLine;
            }
            System.out.println("Password (********): ");
            inputLine = reader.readLine();
            if(inputLine != null && !inputLine.equals("")) {
                password = inputLine;
            } else {
                if(args.length > 0) {
                    password = args[0];
                }
            }
            boolean login = ftp.login(userid, password);
            System.out.println(ftp.getReplyString());
            if(!login) {
                ftp.logout();
                System.out.println(ftp.getReplyString());
                return;
            }
        } catch(Exception e) {
            System.out.println("Could not login to server.");
            if(ftp.isConnected()) {
                try {
                    ftp.disConnect();
                    System.out.println(ftp.getReplyString());
                } catch(IOException ie) { }
            }
            e.printStackTrace();
            return;
        }
        
        //ftp.setSoTimeout(30000);
        //ftp.setDataSocketTimeout(0);
        
        StringTokenizer tokens;
        String command, parm1, parm2, parm3, parm4;
        
        // test functions/commands
        while (true) {
            try {
                System.out.print(dirListingStyle + ":" + host + "-ftp>");
                
                inputLine = reader.readLine();
                tokens = new StringTokenizer(inputLine, " ");
                command = "";
                parm1 = "";
                parm2 = "";
                parm3 = "";
                parm4 = "";
                if (tokens.hasMoreTokens())
                    command = tokens.nextToken();
                if (tokens.hasMoreTokens())
                    parm1 = tokens.nextToken();
                if (tokens.hasMoreTokens())
                    parm2 = tokens.nextToken();
                if (tokens.hasMoreTokens())
                    parm3 = tokens.nextToken();
                if (tokens.hasMoreTokens())
                    parm4 = tokens.nextToken();
                
                // "trace" - setTraceRawCommand()
                if (command.equalsIgnoreCase("trace")) {
                    ftp.setTraceRawCommand(!ftp.isTraceRawCommand());
                    if (ftp.isTraceRawCommand()) {
                        System.out.println("Trace mode is on.");
                    } else {
                        System.out.println("Trace mode is off.");
                    }
                    continue;
                }
                
                // "bye", "quit" - for logout()
                if (command.equalsIgnoreCase("bye")
                || command.equalsIgnoreCase("quit")
                || command.equalsIgnoreCase("logout")) {
                    ftp.logout();
                    System.out.println(ftp.getReplyString());
                    break;
                }
                
                // "ascii" - set transfer file type to ascii - setFileType()
                if (command.equalsIgnoreCase("ascii")
                || command.equalsIgnoreCase("setFileType")
                && parm1.equalsIgnoreCase("ascii")) {
                    ftp.ascii();
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "binary" - set transfer file type to binary - setFileType()
                if (command.equalsIgnoreCase("binary")
                || command.equalsIgnoreCase("setFileType")
                && parm1.equalsIgnoreCase("binary")) {
                    ftp.binary();
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "image" - set transfer file type to image - setFileType()
                if (command.equalsIgnoreCase("image")
                || command.equalsIgnoreCase("setFileType")
                && parm1.equalsIgnoreCase("image")) {
                    ftp.image();
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                
                // "ebcdic" - set transfer file type to ebcdic - setFileType()
                if (command.equalsIgnoreCase("ebcdic")
                || command.equalsIgnoreCase("setFileType")
                && parm1.equalsIgnoreCase("ebcdic")) {
                    ftp.ebcdic();
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                                /*
                                // "local" - set transfer file type to local - setFileType()
                                if (command.equalsIgnoreCase("local")
                                        || command.equalsIgnoreCase("setFileType")
                                        && parm1.equalsIgnoreCase("local")) {
                                        ftp.setFileType(FTP.LOCAL_FILE_TYPE);
                                        //System.out.println(ftp.getReplyString());
                                        continue;
                                }
                                 */
                // "passive" - set connection mode to passive - enterLocalPassiveMode()
                if (command.equalsIgnoreCase("passive")
                || command.equalsIgnoreCase("enterLocalPassiveMode")) {
                    if (ftp.getDataConnectionMode() != FTPClient.PASSIVE_LOCAL_DATA_CONNECTION_MODE) {
                        ftp.usePassive();
                        System.out.println("The connection mode is set to LOCAL PASSIVE.");
                    } else {
                        System.out.println("The connection mode is LOCAL PASSIVE already.");
                    }
                    //System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "active" - set connection mode to passive - enterLocalActiveMode()
                if (command.equalsIgnoreCase("active")
                || command.equalsIgnoreCase("enterLocalActiveMode")) {
                    if (ftp.getDataConnectionMode() != FTPClient.ACTIVE_LOCAL_DATA_CONNECTION_MODE) {
                        ftp.useActive();
                        System.out.println("The connection mode is set to LOCAL ACTIVE.");
                    } else {
                        System.out.println("The connection mode is LOCAL ACTIVE already.");
                    }
                    //System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "verifyRemote" - set RemoteVerification Enabled
                if (command.equalsIgnoreCase("verifyRemote")
                || command.equalsIgnoreCase("setRemoteVerificationEnabled")) {
                    if (ftp.isRemoteVerificationEnabled()) {
                        ftp.setRemoteVerificationEnabled(false);
                        System.out.println("The RemoteVerification is disabled.");
                    } else {
                        System.out.println("The RemoteVerification is enabled.");
                    }
                    continue;
                }
                
                // "system" - getSystemName()
                if (command.equalsIgnoreCase("system")
                || command.equalsIgnoreCase("getSystemName")) {
                    System.out.println(ftp.getSystemName());
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "pwd" - for printWorkingDirectory()
                if (command.equalsIgnoreCase("pwd")
                || command.equalsIgnoreCase("printWorkingDirectory")) {
                    System.out.println(ftp.pwd());
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "cd" - for changeWorkingDirectory()
                if (command.equalsIgnoreCase("cd")
                || command.equalsIgnoreCase("changeWorkingDirectory")) {
                    if (parm1 == null || parm1.equals("")) {
                        System.out.println("Parameter is missing. cd dir.");
                        continue;
                    }
                    ftp.cd(parm1);
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "dir" - for listFiles() -
                // dir [/?] [-F(ile)|-D(ir)|-L(ink)] [-I(ndicate name/size/type)|-R(aw line)]
                //     [pathName] [regExp]
                if (command.equalsIgnoreCase("dir") || command.equalsIgnoreCase("listFiles")) {
                    if (parm1.equals("/?")) {
                        System.out.println(
                                "Usage: dir [/?] [-F(ile)|-D(ir)|-L(ink)] [-I(ndicate name/size/type)|-R(aw line)]\n"
                                + "           [pathName] [regExp]");
                        continue;
                    }
                    String flag = null;
                    String display = "";
                    // filter file type
                    if (parm1.equals("-F") || parm1.equals("-D") || parm1.equals("-L")) {
                        flag = parm1;
                        parm1 = parm2;
                        parm2 = parm3;
                        parm3 = parm4;
                        parm4 = "";
                    }
                    // display name only or raw line?
                    if (parm1.equals("-I") || parm1.equals("-R")) {
                        display = parm1;
                        parm1 = parm2;
                        parm2 = parm3;
                        parm3 = parm4;
                        parm4 = "";
                    }
                    // once again
                    // filter file type
                    if (parm1.equals("-F") || parm1.equals("-D") || parm1.equals("-L")) {
                        flag = parm1;
                        parm1 = parm2;
                        parm2 = parm3;
                        parm3 = parm4;
                        parm4 = "";
                    }
                    // display name only or raw line?
                    if (parm1.equals("-I") || parm1.equals("-R")) {
                        display = parm1;
                        parm1 = parm2;
                        parm2 = parm3;
                        parm3 = parm4;
                        parm4 = "";
                    }
                    FTPFile[] files = null;
                    if (parm1.equals("")) {
                        files = ftp.listFiles();
                    } else if (parm2.equals("")) {
                        files = ftp.listFiles(parm1);
                    } else {
                        files = ftp.listFiles(parm1, parm2);
                    }
                    // filter file type
                    if (flag != null) {
                        int type = (flag.equals("-F") ? 0 : (flag.equals("-D") ? 1 : 2));
                        files = ftp.listFiles(files, type);
                    }
                    if (!ftp.isPositiveCompletion(ftp.getReplyCode()) || files == null) {
                        System.out.println(ftp.getReplyString());
                        continue;
                    }
                    
                    for (int i = 0; i < files.length; i++) {
                        if (display.equals("-R")) {
                            System.out.println(files[i].getRawListing());
                        } else if (display.equals("-I")) {
                            System.out.println(
                                    files[i].getName()
                                    + "    Size is: "
                                    + files[i].getSize()
                                    + "    Type is: "
                                    + (files[i].getType() == 0
                                    ? "file"
                                    : (files[i].getType() == 1 ? "dir" : "link")));
                        } else {
                            System.out.println(files[i].getName());
                        }
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                // "listNames" - for listNames()
                if (command.equalsIgnoreCase("listNames")) {
                    String[] files = null;
                    if (parm1.equals("")) {
                        files = ftp.getDelegate().listNames();
                    } else {
                        files = ftp.getDelegate().listNames(parm1);
                    }
                    if (!ftp.isPositiveCompletion(ftp.getReplyCode()) || files == null) {
                        continue;
                    }
                    for (int i = 0; i < files.length; i++) {
                        System.out.println(files[i]);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "get" - for retrieveFile() - get remote local
                if (command.equalsIgnoreCase("get")
                || command.equalsIgnoreCase("retrieveFile")) {
                    if (parm2.equals("")) {
                        System.out.println("Parameter is missing. get remote local.");
                        continue;
                    }
                    try {
                        if (parm3.equals("")) {
                            ftp.retrieveFile(parm1, parm2);
                        } else {
                            ftp.retrieveFile(parm1, parm2, parm3);
                        }
                    } catch (FileNotFoundException e) {
                        System.out.println(e);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "put" - for storeFile() - put local remote
                if (command.equalsIgnoreCase("put") || command.equalsIgnoreCase("storeFile")) {
                    if (parm2.equals("")) {
                        System.out.println("Parameter is missing. put local remote.");
                        continue;
                    }
                    try {
                        if (parm3.equals("")) {
                            ftp.storeFile(parm2, parm1);
                        } else {
                            ftp.storeFile(parm2, parm3, parm1);
                        }
                    } catch (FileNotFoundException e) {
                        System.out.println(e);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "append" - for appendFile() - append local remote
                if (command.equalsIgnoreCase("append")
                || command.equalsIgnoreCase("appendFile")) {
                    if (parm2.equals("")) {
                        System.out.println("Parameter is missing. append local remote.");
                        continue;
                    }
                    try {
                        if (parm3.equals("")) {
                            ftp.appendFile(parm2, parm1);
                        } else {
                            ftp.appendFile(parm2, parm3, parm1);
                        }
                    } catch (FileNotFoundException e) {
                        System.out.println(e);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // A new empty file will be created on remote site,
                // and then you can write data to it just as working on the normal OutputStream.
                // "new" - for storeFileStream() - new remote
                if (command.equalsIgnoreCase("new") || command.equalsIgnoreCase("storeFileStream")) {
                    if (parm1.equals("")) {
                        System.out.println("Parameter is missing. new remote.");
                        continue;
                    }
                    try {
                        ftp.storeFileStream(parm1);
                        ftp.completePendingCommand();
                    } catch (FileNotFoundException e) {
                        System.out.println(e);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // Open an existing file as an OutputStream,
                // and then you can append data to it just as working on the normal OutputStream.
                // "cat" - for appendFileStream() - cat remote
                if (command.equalsIgnoreCase("cat") || command.equalsIgnoreCase("appendFileStream")) {
                    if (parm1.equals("")) {
                        System.out.println("Parameter is missing. cat remote.");
                        continue;
                    }
                    try {
                        ftp.appendFileStream(parm1);
                        ftp.completePendingCommand();
                    } catch (FileNotFoundException e) {
                        System.out.println(e);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "delete" - for deleteFile()
                if (command.equalsIgnoreCase("delete")) {
                    if (parm1.equals("")) {
                        System.out.println("Parameter is missing. delete file.");
                        continue;
                    }
                    if (parm2.equals("")) {
                        ftp.deleteFile(parm1);
                    } else {
                        ftp.deleteFile(parm1, parm2);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "rename" - for rename() - rename old new
                if (command.equalsIgnoreCase("rename")) {
                    if (parm2.equals("")) {
                        System.out.println("Parameter is missing. rename old new.");
                        continue;
                    }
                    if (parm3.equals("")) {
                        ftp.rename(parm1, parm2);
                    } else {
                        ftp.rename(parm1, parm2, parm3, parm4);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "remotehelp" - for listHelp()
                if (command.equalsIgnoreCase("remotehelp")
                || command.equalsIgnoreCase("listHelp")) {
                    if (parm1.equals("")) {
                        System.out.println(ftp.listHelp());
                    } else {
                        System.out.println(ftp.listHelp(parm1));
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "mkdir" - for makeDirectory()
                if (command.equalsIgnoreCase("mkdir")) {
                    if (parm1.equals("")) {
                        System.out.println("Parameter is missing. mkdir dirname.");
                    } else {
                        ftp.mkdir(parm1);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "mkdirs" - for makeDirectory()
                if (command.equalsIgnoreCase("mkdirs")) {
                    if (parm1.equals("")) {
                        System.out.println("Parameter is missing. mkdirs dirname.");
                    } else {
                        ftp.mkdirs(parm1);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "rmd" - for rmdir()
                if (command.equalsIgnoreCase("rmd") ||
                        command.equalsIgnoreCase("rmdir")) {
                    if (parm1.equals("")) {
                        System.out.println("Parameter is missing. rmd dirname.");
                    } else {
                        ftp.rmdir(parm1);
                    }
                    System.out.println(ftp.getReplyString());
                    continue;
                }
                
                // "raw" - for sendcommand() - send raw ftp command
                if (command.equalsIgnoreCase("raw")
                || command.equalsIgnoreCase("sendcommand")) {
                    ftp.sendCommand(inputLine.substring(command.length()).trim());
                    System.out.println(ftp.getReplyString());
                    //ftp.completePendingCommand();
                    continue;
                }
                
                // not supported commands
                if (!command.equals("")) {
                    System.out.println("Not supported command!");
                }
                
            } catch (Exception e) {
                System.out.println("Got exception.");
                e.printStackTrace();
                break;
            }
        } // while()
        
        if (ftp.isConnected()) {
            try {
                ftp.disConnect();
            } catch (IOException f) {
                // do nothing
            }
        }
        System.out.println("\n\n\n");
        System.out.println("********************************************");
        System.out.println("*                The end!                  *");
        System.out.println("********************************************");
        return;
    }
}
