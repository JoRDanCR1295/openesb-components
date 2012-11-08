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
 * @(#)$Id: HL7CLI.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7.cli;

import com.sun.jbi.hl7.cli.display.Displayer;
import com.sun.jbi.hl7.mgmt.api.HL7ManagementService;
import com.sun.jbi.hl7.mgmt.client.HL7ManagementClient;
import com.sun.jbi.hl7.mgmt.client.HL7ManagementClientFactory;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.management.ObjectName;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;


/**
 *
 * @author ylee
 */
public class HL7CLI {

	public static final String USER = "user";
	public static final String USER_DESC = "user name - default=[admin]";
	public static final String USER_SHORT = "u";

    public static final String PASSWORD = "password";
	public static final String PASSWORD_DESC = "user password";
	public static final String PASSWORD_SHORT = "w";

    public static final String HOST = "host";
	public static final String HOST_DESC = "host name - default=[localhost]";
	public static final String HOST_SHORT = "h";

    public static final String PORT = "port";
	public static final String PORT_DESC = "jmx port number - default=[8686]";
	public static final String PORT_SHORT = "p";

    public static final String REMOTE = "remote";
	public static final String REMOTE_DESC = "use a remote client connection in conjunction with the --host option.";
	public static final String REMOTE_SHORT = "r";

    public static final String TARGET = "target";
	public static final String TARGET_DESC = "name of the target server - default=[server]\n\tFor standalone server profile, specify the server name [server].\n\tFor cluster profile, specify the cluster name [cluster1]";
	public static final String TARGET_SHORT = "t";

    public static final String TARGET_INSTANCE = "targetInstance";
	public static final String TARGET_INSTANCE_DESC = "name of the cluster instance.\n\tFor standalone profile, this option is not required.";
	public static final String TARGET_INSTANCE_SHORT = "i";

    public static final String HELP = "help";
	public static final String HELP_DESC = "print this usage";
	public static final String HELP_SHORT = "?";



    // operations
    public static final String LIST_ENDPOINTS = "list-endpoints";
    public static final String LIST_ENDPOINTS_DESC = "list all endpoints (active and inctive) ";
    public static final String LIST_ENDPOINTS_SHORT = "le";
    public static final String LIST_ENDPOINTS_METHOD_NAME = "listEndpoints";
    public static final String LIST_ENDPOINTS_USAGE = "list-endpoints";

    public static final String GET_ENDPOINTS = "get-endpoints";
    public static final String GET_ENDPOINTS_DESC = "get endpoints for a given service unit";
    public static final String GET_ENDPOINTS_SHORT = "ge";
    public static final String GET_ENDPOINTS_METHOD_NAME = "getEndpoints";
    public static final String GET_ENDPOINTS_USAGE = "list-endpoints <serviceunit>";

    public static final String LIST_ACTIVE_ENDPOINTS = "list-active-endpoints";
    public static final String LIST_ACTIVE_ENDPOINTS_DESC = "list active endpoints";
    public static final String LIST_ACTIVE_ENDPOINTS_SHORT = "lae";
    public static final String LIST_ACTIVE_ENDPOINTS_METHOD_NAME = "listActiveEndpoints";
    public static final String LIST_ACTIVE_ENDPOINTS_USAGE = "list-active-endpoints";

    public static final String LIST_INACTIVE_ENDPOINTS = "list-inactive-endpoints";
    public static final String LIST_INACTIVE_ENDPOINTS_DESC = "list inactive or suspended endpoints";
    public static final String LIST_INACTIVE_ENDPOINTS_SHORT = "lie";
    public static final String LIST_INACTIVE_ENDPOINTS_METHOD_NAME = "listInactiveEndpoints";
    public static final String LIST_INACTIVE_ENDPOINTS_USAGE = "list-inactive-endpoints";

    public static final String LIST_EXTERNAL_CONNECTIONS = "list-external-system-connections";
    public static final String LIST_EXTERNAL_CONNECTIONS_DESC = "list active external system connections (inbound)";
    public static final String LIST_EXTERNAL_CONNECTIONS_SHORT = "lec";
    public static final String LIST_EXTERNAL_CONNECTIONS_METHOD_NAME = "listExternalSystemConnections";
    public static final String LIST_EXTERNAL_CONNECTIONS_USAGE = "list-external-system-connections";

    public static final String LIST_SERVICE_UNITS = "list-service-units";
    public static final String LIST_SERVICE_UNITS_DESC = "list all service units deployed to the HL7 BC";
    public static final String LIST_SERVICE_UNITS_SHORT = "lsu";
    public static final String LIST_SERVICE_UNITS_METHOD_NAME = "listServiceUnits";
    public static final String LIST_SERVICE_UNITS_USAGE = "list-service-units";


    public static final String SUSPEND_ENDPOINT = "suspend-endpoint";
    public static final String SUSPEND_ENDPOINT_DESC = "suspend a given endpoint";
    public static final String SUSPEND_ENDPOINT_SHORT = "se";
    public static final String SUSPEND_ENDPOINT_METHOD_NAME = "suspend";
    public static final String SUSPEND_ENDPOINT_USAGE = "suspend-endpoint <endpoint>";

    public static final String RESUME_ENDPOINT = "resume-endpoint";
    public static final String RESUME_ENDPOINT_DESC = "resume a given endpoint";
    public static final String RESUME_ENDPOINT_SHORT = "re";
    public static final String RESUME_ENDPOINT_METHOD_NAME = "resume";
    public static final String RESUME_ENDPOINT_USAGE = "resume-endpoint <endpoint>";

    public static final String IS_ENDPOINT_ACTIVE = "is-endpoint-active";
    public static final String IS_ENDPOINT_ACTIVE_DESC = "check whether the endpoint is active";
    public static final String IS_ENDPOINT_ACTIVE_SHORT = "iea";
    public static final String IS_ENDPOINT_ACTIVE_METHOD_NAME = "isEndpointActive";
    public static final String IS_ENDPOINT_ACTIVE_USAGE = "is-endpoint-active <endpoint>";

    public static final String STATUS_EXTERNAL_CONNECTIONS = "get-status-external-system-connections";
    public static final String STATUS_EXTERNAL_CONNECTIONS_DESC = "get the status of external client connections (outbound)";
    public static final String STATUS_EXTERNAL_CONNECTIONS_SHORT = "gsesc";
    public static final String STATUS_EXTERNAL_CONNECTIONS_METHOD_NAME = "getExternalSystemConnStatus";
    public static final String STATUS_EXTERNAL_CONNECTIONS_USAGE = "get-status-external-system-connections <serverHostName> <serverPort>";

    public static final String GET_HL7_SERVERS = "list-hl7-servers";
    public static final String GET_HL7_SERVERS_DESC = "retrieve a list of HL7 servers with key value pairs - hostname and port";
    public static final String GET_HL7_SERVERS_SHORT = "lhs";
    public static final String GET_HL7_SERVERS_METHOD_NAME = "getHL7Servers";
    public static final String GET_HL7_SERVERS_USAGE = "list-hl7-servers";


    public static final String MESSAGE_SENT_TIMESTAMP = "get-message-sent-timestamps";
    public static final String MESSAGE_SENT_TIMESTAMP_DESC = "retrieve a list of last sent message timestamps for outbound";
    public static final String MESSAGE_SENT_TIMESTAMP_SHORT = "mst";
    public static final String MESSAGE_SENT_TIMESTAMP_METHOD_NAME = "getMessageSentTimestamps";
    public static final String MESSAGE_SENT_TIMESTAMP_USAGE = "get-message-sent-timestamps";

    public static final String MESSAGE_RECEIVED_TIMESTAMP = "get-message-received-timestamps";
    public static final String MESSAGE_RECEIVED_TIMESTAMP_DESC = "retrieve a list of last message received timestamps for inbound";
    public static final String MESSAGE_RECEIVED_TIMESTAMP_SHORT = "mrt";
    public static final String MESSAGE_RECEIVED_TIMESTAMP_METHOD_NAME = "getMethodReceivedTimestamps";
    public static final String MESSAGE_RECEIVED_TIMESTAMP_USAGE = "get-message-received-timestamps";

    public static final String SEQUENCE_NUMBER = "get-sequence-number";
    public static final String SEQUENCE_NUMBER_DESC = "retrieve the sequence number for a given service unit";
    public static final String SEQUENCE_NUMBER_SHORT = "sn";
    public static final String SEQUENCE_NUMBER_METHOD_NAME = "getSequenceNumber";
    public static final String SEQUENCE_NUMBER_USAGE = "get-sequence-number <serviceunit>";

    public static final Boolean ARG_REQUIRED = Boolean.TRUE;
    public static final Boolean ARG_NOT_REQUIRED = Boolean.FALSE;
    public static final String TAB = "   ";
    public static final String NEWLINE = "\n";
    public static final String COMMA = ",";
    public static final String PREFIX_SHORT = "-";
    public static final String PREFIX_LONG = "--";

    public static final boolean REQUIRED = true;
	public static final boolean NOT_REQUIRED = false;

    private Options options;
	private List<OptionData> optionList = new ArrayList<OptionData>();
    private String port = "8686";
	private String user = "admin";
	private String password = "adminadmin";
	private String host = "localhost";
	private boolean help = false;
    private Map operations;                          // table of operations
    private Operation operation;                     // operation to invoke
    private String operationName;
    private String targetName = "server";
    private String targetInstanceName = null;
    private String remoteConnection = null;



    public HL7CLI() {

        options = new Options();
//		options.addOption(HELP_SHORT, HELP, false, HELP_DESC);
//		options.addOption(USER_SHORT, USER, true, USER_DESC);
//		options.addOption(PASSWORD_SHORT, PASSWORD, true, PASSWORD_DESC);
//		options.addOption(HOST_SHORT, HOST, true, HOST_DESC);
//		options.addOption(PORT_SHORT, PORT, true, PORT_DESC);
//		options.addOption(TARGET_SHORT, TARGET, true, TARGET_DESC);
//		options.addOption(TARGET_INSTANCE_SHORT, TARGET_INSTANCE, true, TARGET_INSTANCE_DESC);

        optionList.add(new OptionData(HELP,HELP_SHORT,HELP_DESC,false));
        optionList.add(new OptionData(USER,USER_SHORT,USER_DESC,true));
        optionList.add(new OptionData(PASSWORD,PASSWORD_SHORT,PASSWORD_DESC,true));
        optionList.add(new OptionData(HOST,HOST_SHORT,HOST_DESC,true));
        optionList.add(new OptionData(PORT,PORT_SHORT,PORT_DESC,true));
        optionList.add(new OptionData(REMOTE,REMOTE_SHORT,REMOTE_DESC,false));
        optionList.add(new OptionData(TARGET,TARGET_SHORT,TARGET_DESC,true));
        optionList.add(new OptionData(TARGET_INSTANCE,TARGET_INSTANCE_SHORT,TARGET_INSTANCE_DESC,true));

        addOptions();

        // construct operations table map
        operations = new LinkedHashMap<String, Operation>();
        operations.put(LIST_SERVICE_UNITS,
                new Operation(
                LIST_SERVICE_UNITS,
                LIST_SERVICE_UNITS_SHORT,
                LIST_SERVICE_UNITS_DESC,
                LIST_SERVICE_UNITS_METHOD_NAME,
                ARG_NOT_REQUIRED,
                LIST_SERVICE_UNITS_USAGE,
                0));
        operations.put(LIST_ENDPOINTS, new Operation(LIST_ENDPOINTS,
                LIST_ENDPOINTS_SHORT,
                LIST_ENDPOINTS_DESC,
                LIST_ENDPOINTS_METHOD_NAME,
                ARG_NOT_REQUIRED,
                LIST_ENDPOINTS_USAGE,
                0));
        operations.put(GET_ENDPOINTS, new Operation(GET_ENDPOINTS,
                GET_ENDPOINTS_SHORT,
                GET_ENDPOINTS_DESC,
                GET_ENDPOINTS_METHOD_NAME,
                ARG_REQUIRED,
                GET_ENDPOINTS_USAGE,
                1));
        operations.put(LIST_ACTIVE_ENDPOINTS,
                new Operation(LIST_ACTIVE_ENDPOINTS,
                LIST_ACTIVE_ENDPOINTS_SHORT, 
                LIST_ACTIVE_ENDPOINTS_DESC, 
                LIST_ACTIVE_ENDPOINTS_METHOD_NAME, 
                ARG_NOT_REQUIRED,
                LIST_ACTIVE_ENDPOINTS_USAGE,
                0));
        operations.put(LIST_INACTIVE_ENDPOINTS, 
                new Operation(
                LIST_INACTIVE_ENDPOINTS,
                LIST_INACTIVE_ENDPOINTS_SHORT,
                LIST_INACTIVE_ENDPOINTS_DESC,
                LIST_INACTIVE_ENDPOINTS_METHOD_NAME,
                ARG_NOT_REQUIRED,
                LIST_INACTIVE_ENDPOINTS_USAGE,
                0));
        operations.put(LIST_EXTERNAL_CONNECTIONS, 
                new Operation(
                LIST_EXTERNAL_CONNECTIONS,
                LIST_EXTERNAL_CONNECTIONS_SHORT,
                LIST_EXTERNAL_CONNECTIONS_DESC,
                LIST_EXTERNAL_CONNECTIONS_METHOD_NAME,
                ARG_NOT_REQUIRED,
                LIST_EXTERNAL_CONNECTIONS_USAGE,
                0));
        operations.put(GET_HL7_SERVERS,
                new Operation(
                GET_HL7_SERVERS,
                GET_HL7_SERVERS_SHORT,
                GET_HL7_SERVERS_DESC,
                GET_HL7_SERVERS_METHOD_NAME,
                ARG_NOT_REQUIRED,
                GET_HL7_SERVERS_USAGE,
                0));
        operations.put(SUSPEND_ENDPOINT, 
                new Operation(
                SUSPEND_ENDPOINT,
                SUSPEND_ENDPOINT_SHORT,
                SUSPEND_ENDPOINT_DESC,
                SUSPEND_ENDPOINT_METHOD_NAME,
                ARG_REQUIRED,
                SUSPEND_ENDPOINT_USAGE,
                1));
        operations.put(RESUME_ENDPOINT, 
                new Operation(
                RESUME_ENDPOINT,
                RESUME_ENDPOINT_SHORT,
                RESUME_ENDPOINT_DESC,
                RESUME_ENDPOINT_METHOD_NAME,
                ARG_REQUIRED,
                RESUME_ENDPOINT_USAGE,
                1));
        operations.put(IS_ENDPOINT_ACTIVE,
                new Operation(
                IS_ENDPOINT_ACTIVE,
                IS_ENDPOINT_ACTIVE_SHORT,
                IS_ENDPOINT_ACTIVE_DESC,
                IS_ENDPOINT_ACTIVE_METHOD_NAME,
                ARG_REQUIRED,
                IS_ENDPOINT_ACTIVE_USAGE,
                1));
        operations.put(STATUS_EXTERNAL_CONNECTIONS, 
                new Operation(
                STATUS_EXTERNAL_CONNECTIONS,
                STATUS_EXTERNAL_CONNECTIONS_SHORT,
                STATUS_EXTERNAL_CONNECTIONS_DESC,
                STATUS_EXTERNAL_CONNECTIONS_METHOD_NAME,
                ARG_REQUIRED,
                STATUS_EXTERNAL_CONNECTIONS_USAGE,
                2));
        operations.put(MESSAGE_SENT_TIMESTAMP, 
                new Operation(
                MESSAGE_SENT_TIMESTAMP,
                MESSAGE_SENT_TIMESTAMP_SHORT,
                MESSAGE_SENT_TIMESTAMP_DESC,
                MESSAGE_SENT_TIMESTAMP_METHOD_NAME,
                ARG_NOT_REQUIRED,
                MESSAGE_SENT_TIMESTAMP_USAGE,
                0));
        operations.put(MESSAGE_RECEIVED_TIMESTAMP, 
                new Operation(
                MESSAGE_RECEIVED_TIMESTAMP,
                MESSAGE_RECEIVED_TIMESTAMP_SHORT,
                MESSAGE_RECEIVED_TIMESTAMP_DESC,
                MESSAGE_RECEIVED_TIMESTAMP_METHOD_NAME,
                ARG_NOT_REQUIRED,
                MESSAGE_RECEIVED_TIMESTAMP_USAGE,
                0));
        operations.put(SEQUENCE_NUMBER, 
                new Operation(
                SEQUENCE_NUMBER,
                SEQUENCE_NUMBER_SHORT,
                SEQUENCE_NUMBER_DESC,
                SEQUENCE_NUMBER_METHOD_NAME,                
                ARG_REQUIRED,
                SEQUENCE_NUMBER_USAGE,
                1));

        operation = new Operation();

    }


    private void addOptions() {
        for ( OptionData optionData : optionList ) {
    		options.addOption(optionData.getShortName(), optionData.getName(), optionData.hasArg(), optionData.getDesc());
        }
    }

    public void parseArguments(HL7CLI cli, String args[]) {
        CommandLineParser parser = new PosixParser();
		try {
			CommandLine line = parser.parse(options, args);
            //System.out.println("line: "+line.getArgList());
			cli.help = getBooleanOption(line, HELP_SHORT, HELP);
			if (cli.help) {
				return;
			}
			String user = getStringOption(line, USER_SHORT, USER,
					 NOT_REQUIRED);
            if ( user!=null ) {
                cli.user = user;
            }
            String password = getStringOption(line, PASSWORD_SHORT, PASSWORD,
					NOT_REQUIRED);
            if ( password!=null ) {
                cli.password = password;
            }
			String host = getStringOption(line, HOST_SHORT, HOST, NOT_REQUIRED);
            if ( host!=null ) {
                cli.host = host;
            }
			String port = getIntOption(line, PORT_SHORT, PORT, NOT_REQUIRED);
            if ( port !=null ) {
                cli.port = port;
            }
			String target = getStringOption(line, TARGET_SHORT, TARGET, NOT_REQUIRED);
            if ( target !=null ) {
                cli.targetName = target;
            }
			String instance = getStringOption(line, TARGET_INSTANCE_SHORT, TARGET_INSTANCE, NOT_REQUIRED);
            if ( instance !=null ) {
                cli.targetInstanceName = instance;
            }
			boolean remote = getBooleanOption(line, REMOTE_SHORT, REMOTE);
            if ( remote ) {
                cli.remoteConnection = "true";
            }
            // parse operation
            getOperation(line,cli);

		} catch (Exception e) {
			System.err.println(e.getMessage());
			printUsage();
			System.exit(2);
		}

    }


	public String getStringOption(CommandLine line, String c, String opt,
			boolean required) throws Exception {
		String r = line.getOptionValue(c);
		if (r == null) {
			r = line.getOptionValue(opt);
		}
		if (required) {
			validateStringOption(r, opt);
		}
		return r;
	}

    public void validateStringOption(String param, String option)
			throws Exception {
		if (param == null) {
			throw new Exception("Invalid value for parameter " + option + "["
					+ param + "]. Parameter is required");
		}
	}

	public boolean getBooleanOption(CommandLine line, String c, String opt) {
		boolean hasOption = false;
		hasOption = line.hasOption(c);
		if (!hasOption) {
			hasOption = line.hasOption(opt);
		}
		return hasOption;
	}

	public String getIntOption(CommandLine line, String c, String opt,
			boolean required) throws Exception {
		String p = getStringOption(line, c, opt, required);
		if (p != null) {
			Integer.parseInt(p);
		}
		return p;
	}

    public void getOperation(CommandLine line, HL7CLI cli) {
        List<String> argsList = line.getArgList();
        if (argsList != null && !argsList.isEmpty()) {
            operationName = argsList.get(0);
            Operation op = (Operation) operations.get(operationName);
            if (op != null) {
                operation.copy(op);
                if (op.hasArgs()) {
                    // get all the args
                    for (int i = 1; i < argsList.size(); i++) {
                        String arg = argsList.get(i);
                        operation.addArg(arg);
                    }
                }
                //System.out.println(operation.toString());
            } else {
               System.out.println(operationName+" not defined. Processing aborted!");
            }
        }
    }

    public void printUsage() {
        //HelpFormatter formatter = new HelpFormatter();
        //formatter.printHelp(this.getClass().getCanonicalName()+" [options]", options);
        //formatter.printHelp(this.getClass().getSimpleName()+" [options] operation", options);
        System.out.println("Usage: "+this.getClass().getSimpleName()+" [options] operation [args...]");

        System.out.println("\n where options include:");
        for ( OptionData optionData : optionList) {
            System.out.println(optionData.printUsage());
        }

        System.out.println("\n where operations include:");
        for( Iterator opIter=operations.values().iterator(); opIter.hasNext(); ) {
            Operation op = (Operation)opIter.next();
            System.out.println(op.printUsage());
        }
    }


    public Object execute(HL7CLI cli) {
        System.out.println(operation);
        if (operation.validate()) {
            HL7ManagementClient client = getClient(cli);
            HL7ManagementService service = client.getHL7ManagementService();
            Object result = null;
            try {
                if (GET_ENDPOINTS_METHOD_NAME.equals(operation.getMethodName())) {
                    String serviceUnit = operation.getArg(0);
                    result = service.getEndpoints(serviceUnit, cli.targetName, cli.targetInstanceName);
                } else if (LIST_ENDPOINTS_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.listEndpoints(cli.targetName, cli.targetInstanceName);
                } else if (LIST_ACTIVE_ENDPOINTS_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.listActiveEndpoints(cli.targetName, cli.targetInstanceName);
                } else if (LIST_INACTIVE_ENDPOINTS_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.listInactiveEndpoints(cli.targetName, cli.targetInstanceName);
                } else if (LIST_EXTERNAL_CONNECTIONS_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.listExternalSystemConnections(cli.targetName, cli.targetInstanceName);
                } else if (LIST_SERVICE_UNITS_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.listServiceUnits(cli.targetName, cli.targetInstanceName);
                } else if (GET_HL7_SERVERS_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.getHl7Servers(cli.targetName, cli.targetInstanceName);
                } else if (STATUS_EXTERNAL_CONNECTIONS_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    String serverLocation = cli.operation.getArg(0);
                    String serverPort = cli.operation.getArg(1);
                    result = service.getExternalSystemConnectionStatus(serverLocation, Long.parseLong(serverPort), cli.targetName, cli.targetInstanceName);
                } else if (SUSPEND_ENDPOINT_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    String endpoint = cli.operation.getArg(0);
                    result = service.suspend(endpoint, cli.targetName, cli.targetInstanceName);
                } else if (RESUME_ENDPOINT_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    String endpoint = cli.operation.getArg(0);
                    result = service.resume(endpoint, cli.targetName, cli.targetInstanceName);
                } else if (IS_ENDPOINT_ACTIVE_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    String endpoint = cli.operation.getArg(0);
                    result = service.isEndpointActive(endpoint, cli.targetName, cli.targetInstanceName);
                } else if (MESSAGE_SENT_TIMESTAMP_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.getMessageSentTimestamps(cli.targetName, cli.targetInstanceName);
                } else if (MESSAGE_RECEIVED_TIMESTAMP_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    result = service.getMessageReceivedTimestamps(cli.targetName, cli.targetInstanceName);
                } else if (SEQUENCE_NUMBER_METHOD_NAME.equals(cli.operation.getMethodName())) {
                    String endpoint = cli.operation.getArg(0);
                    result = service.getSequenceNumber(endpoint, cli.targetName, cli.targetInstanceName);
                } else {
                    if ( cli.operation.getName()==null ) {
                        System.out.println("Error: no operation specified. Please use the --help option to see usage." );
                    } else {
                        System.out.println("Error: " + operation.getName() + " not supported.");
                    }
                }
                if (result != null) {
                    System.out.println("result:");
                    display(result);
                //System.out.println("result: "+result.toString());
                }
            } catch (Exception e) {
                //System.out.println( operation.getName()+ " " + e);
                e.printStackTrace();
            }
        } else {
            System.out.println("Error: validation failed! - see usage\n"+cli.operation.printUsage());
        }

        return "";
    }

	public void display(Object obj) {
		Displayer displayer = new Displayer();
		if (displayer != null) {
			displayer.display(obj);
		} else {
			if (obj != null) {
				System.out.println("Displayer not found for:"
						+ obj.getClass().getName());
				System.out.println(obj + "");
			}
		}
	}

    public HL7ManagementClient getClient(HL7CLI cli) {
       HL7ManagementClient client = null;
        try {
            if ( cli.remoteConnection!=null ) {
                // force a remote connection connection
                client = HL7ManagementClientFactory.getInstance(cli.host, Integer.parseInt(cli.port), cli.user, cli.password,true);
            } else {
                client = HL7ManagementClientFactory.getInstance(cli.host, Integer.parseInt(cli.port), cli.user, cli.password);
            }
        } catch (Exception ex) {
            System.out.println("Exception in connecting to Runtime *******" + ex.getMessage());
        }
        return client;
    }

    public ObjectName getMBean() {
        ObjectName objName = null;
        try {
            objName = new ObjectName("com.sun.jbi:JbiName=server,CustomControlName=Administration,ComponentName=sun-hl7-binding,ControlType=Custom,ComponentType=Installed,InstalledType=Binding");
        } catch(Exception e) {
            e.printStackTrace();
        }
        return objName;
    }

    public static void main(String[] args) {
		String line = "";
		if (args != null) {
			for (int i = 0; i < args.length; i++) {
				line += " " + args[i];
			}
		}
		try {
			HL7CLI cli = new HL7CLI();
            if ( args.length <1 ) {
                cli.printUsage();
                return;
            }
			line = cli.getClass().getName() + line;
			cli.parseArguments(cli,args);

			if (cli.help) {
				cli.printUsage();
				return;
			}

            cli.execute(cli);

		} catch (Exception e) {
				System.out.println("ERROR executing command:\n\n" + line +"\n");
				System.out.println("ERROR:" + e.getMessage() + "\n");
		}
		//System.out.println("Leaving HL7CLI.");
    }





}
