package it.imolinfo.jbi4corba.test.rtti;


/**
* it/imolinfo/jbi4corba/test/rtti/_rtti_testStub.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from RttiPragmaPrefix.idl
* Wednesday, July 22, 2009 4:13:00 PM CEST
*/

public class _rtti_testStub extends org.omg.CORBA.portable.ObjectImpl implements it.imolinfo.jbi4corba.test.rtti.rtti_test
{


  //union
  public String enum_echo1 (rttitypes.ThirdUnion msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo1", true);
                rttitypes.ThirdUnionHelper.write ($out, msg);
                $in = _invoke ($out);
                String $result = $in.read_string ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo1 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo1

  public rttitypes.ThirdUnion enum_echo2 (rttitypes.SecondUnionHolder msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo2", true);
                rttitypes.SecondUnionHelper.write ($out, msg.value);
                $in = _invoke ($out);
                rttitypes.ThirdUnion $result = rttitypes.ThirdUnionHelper.read ($in);
                msg.value = rttitypes.SecondUnionHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo2 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo2

  public rttitypes.ComplexStruct1 enum_echo3 (String msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo3", true);
                $out.write_string (msg);
                $in = _invoke ($out);
                rttitypes.ComplexStruct1 $result = rttitypes.ComplexStruct1Helper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo3 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo3

  public String enum_echo4 (rttitypes.ComplexStruct1 msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo4", true);
                rttitypes.ComplexStruct1Helper.write ($out, msg);
                $in = _invoke ($out);
                String $result = $in.read_string ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo4 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo4

  public String enum_echo5 (rttitypes.ComplexStruct1Holder msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo5", true);
                rttitypes.ComplexStruct1Helper.write ($out, msg.value);
                $in = _invoke ($out);
                String $result = $in.read_string ();
                msg.value = rttitypes.ComplexStruct1Helper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo5 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo5

  public rttitypes.ComplexStruct1 enum_echo6 (String msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo6", true);
                $out.write_string (msg);
                $in = _invoke ($out);
                rttitypes.ComplexStruct1 $result = rttitypes.ComplexStruct1Helper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo6 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo6

  public rttitypes.ComplexStruct1[][] enum_echo7 (String msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo7", true);
                $out.write_string (msg);
                $in = _invoke ($out);
                rttitypes.ComplexStruct1 $result[][] = rttitypes.ArrayComplexStruct1Helper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo7 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo7

  public rttitypes.SecondUnion enum_echo8 (String msg) throws rttitypes.EchoUnionException
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo8", true);
                $out.write_string (msg);
                $in = _invoke ($out);
                rttitypes.SecondUnion $result = rttitypes.SecondUnionHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                if (_id.equals ("IDL:3hh4.123/rttitypes/EchoUnionException:1.0"))
                    throw rttitypes.EchoUnionExceptionHelper.read ($in);
                else
                    throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo8 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo8

  public String enum_echo9 (rttitypes.ComplexStruct1[][] msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("enum_echo9", true);
                rttitypes.ArrayComplexStruct1Helper.write ($out, msg);
                $in = _invoke ($out);
                String $result = $in.read_string ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return enum_echo9 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // enum_echo9


  //enum
  public rttitypes.EchoComplexEnum echoEnum (rttitypes.EchoComplexEnum e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoEnum", true);
                rttitypes.EchoComplexEnumHelper.write ($out, e);
                $in = _invoke ($out);
                rttitypes.EchoComplexEnum $result = rttitypes.EchoComplexEnumHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoEnum (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoEnum


  //any
  public String any_echo1 (org.omg.CORBA.AnyHolder msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("any_echo1", true);
                $out.write_any (msg.value);
                $in = _invoke ($out);
                String $result = $in.read_string ();
                msg.value = $in.read_any ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return any_echo1 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // any_echo1

  public org.omg.CORBA.Any any_echo2 (org.omg.CORBA.Any msg) throws rttitypes.EchoAnyException
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("any_echo2", true);
                $out.write_any (msg);
                $in = _invoke ($out);
                org.omg.CORBA.Any $result = $in.read_any ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                if (_id.equals ("IDL:3hh4.123/rttitypes/EchoAnyException:1.0"))
                    throw rttitypes.EchoAnyExceptionHelper.read ($in);
                else
                    throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return any_echo2 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // any_echo2

  public rttitypes.ComplexStruct2 any_echo3 (rttitypes.TempUnion1 msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("any_echo3", true);
                rttitypes.TempUnion1Helper.write ($out, msg);
                $in = _invoke ($out);
                rttitypes.ComplexStruct2 $result = rttitypes.ComplexStruct2Helper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return any_echo3 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // any_echo3

  public org.omg.CORBA.Any[][] any_echo4 (rttitypes.ComplexStruct2Holder msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("any_echo4", true);
                rttitypes.ComplexStruct2Helper.write ($out, msg.value);
                $in = _invoke ($out);
                org.omg.CORBA.Any $result[][] = rttitypes.ArrayOfAnyHelper.read ($in);
                msg.value = rttitypes.ComplexStruct2Helper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return any_echo4 (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // any_echo4

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:3yy4.321/it/imolinfo/jbi4corba/test/rtti/rtti_test:1.0"};

  public String[] _ids ()
  {
    return (String[])__ids.clone ();
  }

  private void readObject (java.io.ObjectInputStream s) throws java.io.IOException
  {
     String str = s.readUTF ();
     String[] args = null;
     java.util.Properties props = null;
     org.omg.CORBA.Object obj = org.omg.CORBA.ORB.init (args, props).string_to_object (str);
     org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl) obj)._get_delegate ();
     _set_delegate (delegate);
  }

  private void writeObject (java.io.ObjectOutputStream s) throws java.io.IOException
  {
     String[] args = null;
     java.util.Properties props = null;
     String str = org.omg.CORBA.ORB.init (args, props).object_to_string (this);
     s.writeUTF (str);
  }
} // class _rtti_testStub
