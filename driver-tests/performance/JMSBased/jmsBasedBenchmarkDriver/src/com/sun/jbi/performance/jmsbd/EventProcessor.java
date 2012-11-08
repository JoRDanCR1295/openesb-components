/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.performance.jmsbd;

import com.sun.jbi.engine.iep.core.runtime.DefaultIEPEngine;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 *
 * @author Bing Lu
 */
public class EventProcessor {
    private Properties mProperties;
    private DefaultIEPEngine mEngine;
    private Map<String, String> mInput2Queue = new HashMap<String, String>();
    private Map<String, String> mOutput2Queue = new HashMap<String, String>();
    private QueryPlan mPlan;
    private String mInstanceId;
    private String mIepFileName;
    private String mIepFileFullPath;
    private List<EventCollector> mEvtCollectorList = new ArrayList<EventCollector>();
    private List<EventNotifier> mEvtNotifierList = new ArrayList<EventNotifier>();

    public EventProcessor(Properties properties) throws Exception {
        mProperties = properties;
        mEngine = DefaultIEPEngine.getInstance();
        mEngine.init(properties);

        mIepFileFullPath = mProperties.getProperty("iep.file");
        File iepFile = new File(mIepFileFullPath);
        mIepFileName = iepFile.getName();
        mInstanceId = mIepFileName.substring(0, mIepFileName.length()-4); // .iep

        int inputCnt = PropertyUtil.getint(mProperties, "iep.input.count", 0);
        for (int i = 0; i < inputCnt; i++) {
            String operator = mProperties.getProperty("iep.input." + i + ".operator");
            String queue = mProperties.getProperty("iep.input." + i + ".queue");
            mInput2Queue.put(operator, queue);
        }
        int outputCnt = PropertyUtil.getint(mProperties, "iep.output.count", 0);
        for (int i = 0; i < outputCnt; i++) {
            String operator = mProperties.getProperty("iep.output." + i + ".operator");
            String queue = mProperties.getProperty("iep.output." + i + ".queue");
            mOutput2Queue.put(operator, queue);
        }

        mEngine.deployFile(mInstanceId, mIepFileName, mIepFileFullPath);
        System.out.println(mIepFileFullPath + " is successfully deployed as: " + mInstanceId);
        mPlan = Util.getPlanByInstanceId(mProperties, mInstanceId);
        for (Map.Entry<String, String> entry : mInput2Queue.entrySet()) {
            Operator operator = mPlan.getOperatorByName(entry.getKey());
            String queueName = entry.getValue();
            EventCollector ec = new EventCollector(operator, queueName, mProperties);
            mEvtCollectorList.add(ec);
        }
        for (Map.Entry<String, String> entry : mOutput2Queue.entrySet()) {
            Operator operator = mPlan.getOperatorByName(entry.getKey());
            String queueName = entry.getValue();
            EventNotifier en = new EventNotifier(mPlan, operator, queueName, mProperties);
            mEvtNotifierList.add(en);
        }
        for (EventCollector ec : mEvtCollectorList) {
            ec.start();
        }
        for (EventNotifier en : mEvtNotifierList) {
            en.start();
        }
    }

    public void redeploy() throws Exception {
        mEngine.undeploy(mInstanceId);
        mEngine.deployFile(mInstanceId, mIepFileName, mIepFileFullPath);
    }

    public void start() throws Exception {
        mEngine.start(mInstanceId);
    }

    public void stop() throws Exception {
        mEngine.stop(mInstanceId);
    }

    public void undeploy() throws Exception {
        mEngine.undeploy(mInstanceId);
    }

    public void quit(boolean clean) throws Exception {
        for (EventCollector ec : mEvtCollectorList) {
            ec.stopAndWait();
        }
        for (EventNotifier en : mEvtNotifierList) {
            en.stopAndWait();
        }
        mEngine.quit(clean);
    }

    private static String[] parseCommand(String command) {
        StringTokenizer st = new StringTokenizer(command, " ");
        List<String> list = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            list.add(st.nextToken());
        }
        return (String[])list.toArray(new String[0]);
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("java com.sun.jbi.performance.jmsbd.EventProcessor event-processor.properties");
            return;
        }
        String configFileName = args[0];
        //Load the properties to resolve relative path of logFile and inputTemplateFile to absolute path
        File configFile = new File(configFileName);
        String path = configFile.getParentFile().getAbsolutePath();
        Properties configProp = new Properties();
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(configFile);
            configProp.load(fis);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (Exception e) {
                }
            }
        }
        configProp.put("iep.file", path + File.separator + configProp.getProperty("iep.file"));
        EventProcessor mInstance;
        try {
            mInstance = new EventProcessor(configProp);
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
        StringBuffer menu = new StringBuffer();
        menu.append("\n>action? ");
        menu.append("(redeploy)");
        menu.append("(undeploy)");
        menu.append("|(start)");
        menu.append("|(stop)");
        menu.append("|(quit)");
        menu.append("|(clean-quit)");
        BufferedReader userIn = null;
        try {
            userIn = new BufferedReader(new InputStreamReader(System.in));
            while (true) {
                System.out.println(menu.toString());
                System.out.print(">>");
                String ans = userIn.readLine();
                if (ans == null) {
                    break;
                }
                if (ans.trim().equals("")) {
                    continue;
                }
                String[] cmd = parseCommand(ans);
                if ("redeploy".startsWith(cmd[0])) {
                    mInstance.redeploy();
                } else if ("undeploy".startsWith(cmd[0])) {
                    mInstance.undeploy();
                } else if ("start".startsWith(cmd[0])) {
                    mInstance.start();
                } else if ("stop".startsWith(cmd[0])) {
                    mInstance.stop();
                } else if ("quit".startsWith(cmd[0])) {
                    mInstance.quit(false);
                    System.exit(0);
                } else if ("clean-quit".startsWith(cmd[0])) {
                    mInstance.quit(true);
                    System.exit(0);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (userIn != null) {
                    userIn.close();
                }
            } catch (Exception e1) {
            }
        }
    }

}
