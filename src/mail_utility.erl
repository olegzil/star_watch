-module(mail_utility).
-export([send_email/2]).
-include("include/macro_definitions.hrl").

send_email(ToEmail, ToName) ->
    % Set the necessary variables
    {ApiKeyPublic, ApiKeyPrivate} = server_config_processor:get_email_keys(?SERVER_CONFIG_FILE),
    Url = "https://api.mailjet.com/v3.1/send",
    ContentType = "application/json",
    SandboxMode = false,
    FromEmail = <<"TheTinkerersShop@gmail.com">>,
    FromName = <<"Your Mailjet Pilot">>,
    HTMLPart = <<"<h3>Dear passenger, welcome to Mailjet!</h3><br />May the delivery force be with you!">>,
    Subject = <<"Your email flight plan!">>,
    TextPart = <<"Dear passenger, welcome to Mailjet! May the delivery force be with you!">>,

    % Create the request payload
    Payload = #{<<"SandboxMode">> => SandboxMode,
                <<"Messages">> => [
                    #{<<"From">> => #{<<"Email">> => FromEmail, <<"Name">> => FromName},
                      <<"To">> => [#{<<"Email">> => ToEmail, <<"Name">> => ToName}],
                      <<"Subject">> => Subject,
                      <<"TextPart">> => TextPart,
                      <<"HTMLPart">> => HTMLPart
                    }
                ]
              },

    % Convert the payload to JSON
    JsonPayload = binary_to_list(jiffy:encode(Payload)),

    % Construct the HTTP request
    Options = [{ssl, [{verify, verify_none}]}],
    Headers = [{"Content-Type", ContentType},
               {"Authorization", "Basic " ++ base64:encode_to_string(ApiKeyPublic ++ ":" ++ ApiKeyPrivate)}],
    Request = {Url, Headers, "POST", JsonPayload},

    % Perform the HTTP request
    {ok, {StatusCode, _RespHeaders, RespBody}} = httpc:request(post, Request, Options, [{body_format, binary}]),

    % Handle the response
    io:format("Status code: ~p~n", [StatusCode]),
    io:format("Response Body: ~p~n", [RespBody]).
