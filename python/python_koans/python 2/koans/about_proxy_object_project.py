#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Project: Create a Proxy Class
#
# In this assignment, create a proxy class (one is started for you
# below).  You should be able to initialize the proxy object with any
# object.  Any attributes called on the proxy object should be forwarded
# to the target object.  As each attribute call is sent, the proxy should
# record the name of the attribute sent.
#
# The proxy class is started for you.  You will need to add a method
# missing handler and any other supporting methods.  The specification
# of the Proxy class is given in the AboutProxyObjectProject koan.

# Note: This is a bit trickier that its Ruby Koans counterpart, but you
# can do it!

from runner.koan import *
from collections import Counter

class Proxy(object):
    """Proxy class wraps any other class, and adds functionality to remember and report all messages called.
    Limitations include that proxy blocks all direct subclass calls to:
    messages, number_of_times_called, was_called, _obj, and _message_counts.
    These calls must be made directly like my_proxy_instance._obj.messages.
    """


    def __init__(self, target_object):
        print 'initializing a proxy for ' + target_object.__class__.__name__
        # WRITE CODE HERE
        self._message_counts = Counter();
        #initialize '_obj' attribute last. Trust me on this!
        self._obj = target_object

    # WRITE CODE HERE                                   
    def  __getattr__(self, attr_name):
        print 'getting an attribute: "' + attr_name + '" from "' + self._obj.__class__.__name__  + '"'
        self._message_counts[attr_name] += 1
        print self._message_counts
        return object.__getattribute__(self._obj, attr_name)

    #def __getattribute__(self, attr_name):
    #    print "intercepted!~ " + attr_name
    #    object.__getattribute__(self, attr_name)

    def __setattr__(self, attr_name, value):
        if((attr_name == '_obj') | (attr_name == '_message_counts')): # special proxy attributes.
            print 'setting the PROXY attribute: "' + attr_name + '"'
            object.__setattr__(self, attr_name, value)
        else:
            print 'setting the REAL attribute: "' + attr_name + '"'
            self._message_counts[attr_name+"="] += 1
            object.__setattr__(self._obj, attr_name, value)

    def messages(self):
        return self._message_counts.keys()
    
    def number_of_times_called(self, attr_name):
        return self._message_counts[attr_name]

    def was_called(self, attr_name):
        return attr_name in self._message_counts

# The proxy object should pass the following Koan:
#
class AboutProxyObjectProject(Koan):

    def test_proxy_method_returns_wrapped_object(self):
        # NOTE: The Television class is defined below
        tv = Proxy(Television())
        
        self.assertTrue(isinstance(tv, Proxy))
    
    def test_tv_methods_still_perform_their_function(self):
        tv = Proxy(Television())
        
        tv.channel = 10
        tv.power()
        self.assertEqual(10, tv.channel)
        
        self.assertTrue(tv.is_on())
    
    def test_proxy_records_messages_sent_to_tv(self):
        print "test started"
        tv = Proxy(Television())
        
        tv.channel = 10
        tv.power()
        
        self.assertEqual(['power', 'channel='], tv.messages())
    
    def test_proxy_handles_invalid_messages(self):
        tv = Proxy(Television())
        
        ex = None
        try:
            tv.no_such_method()
        except AttributeError as ex:
            pass

        self.assertEqual(AttributeError, type(ex))
        
    
    def test_proxy_reports_methods_have_been_called(self):
        tv = Proxy(Television())
        
        tv.power()
        tv.power()
        
        self.assertTrue(tv.was_called('power'))
        self.assertFalse(tv.was_called('channel'))
    
    def test_proxy_counts_method_calls(self):
        tv = Proxy(Television())
        
        tv.power()
        tv.channel = 48
        tv.power()
      
        self.assertEqual(2, tv.number_of_times_called('power'))
        self.assertEqual(1, tv.number_of_times_called('channel='))
        self.assertEqual(0, tv.number_of_times_called('is_on'))
    
    def test_proxy_can_record_more_than_just_tv_objects(self):
        proxy = Proxy("Py Ohio 2010")
      
        result = proxy.upper()

        self.assertEqual("PY OHIO 2010", result)
        
        result = proxy.split()

        self.assertEqual(["Py", "Ohio", "2010"], result)
        self.assertEqual(['upper', 'split'], proxy.messages())

# ====================================================================
# The following code is to support the testing of the Proxy class.  No
# changes should be necessary to anything below this comment.

# Example class using in the proxy testing above.
class Television(object):
    def __init__(self):
        self._channel = None
        self._power = None
        
    @property
    def channel(self):
        print "TV CHANNEL ACCESS"
        return self._channel

    @channel.setter
    def channel(self, value):
        print "CHANGING THE CHANNEL"
        self._channel = value        
        
    def power(self):
        if self._power == 'on':
            self._power = 'off'
        else:
            self._power = 'on'
    
    def is_on(self):
        return self._power == 'on'

# Tests for the Television class.  All of theses tests should pass.
class TelevisionTest(Koan):
    def test_it_turns_on(self):
        tv = Television()
        
        tv.power()
        self.assertTrue(tv.is_on())
    
    def test_it_also_turns_off(self):
        tv = Television()
        
        tv.power()
        tv.power()
        
        self.assertFalse(tv.is_on())
    
    def test_edge_case_on_off(self):
        tv = Television()
        
        tv.power()
        tv.power()
        tv.power()
            
        self.assertTrue(tv.is_on())
        
        tv.power()
        
        self.assertFalse(tv.is_on())
  
    def test_can_set_the_channel(self):
        tv = Television()
    
        tv.channel = 11
        self.assertEqual(11, tv.channel)
