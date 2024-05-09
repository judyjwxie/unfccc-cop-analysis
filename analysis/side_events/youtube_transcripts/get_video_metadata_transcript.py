from pytube import YouTube
from youtube_transcript_api import YouTubeTranscriptApi
import pandas as pd 
import numpy as np

# THIS SCRIPT extracts the English transcript of videos 
# and exports the metadata of the videos to a csv

# https://pytube.io/en/latest/user/captions.html?highlight=transcript#subtitle-caption-tracks
# pytude does have a caption feature, but maybe it doesn't read the autogenerated ones
# description of pytube features: https://pytube.io/en/latest/api.html

# use youtube transcript api to get the auto-generated language
# https://pypi.org/project/youtube-transcript-api/

# create a data frame that stores the metadata
with open('data/side_events/youtube_transcripts/youtube_video_links.txt') as f:
    lines = f.readlines()
video_metadata = pd.DataFrame(data={"link":lines})


for i in np.arange(len(lines)):
    video_url = lines[i]
    yt = YouTube(video_url)
    # get video simple metadata
    video_metadata.loc[i,"title"] = yt.title
    video_metadata.loc[i,"publish_date"] = yt.publish_date
    stream = yt.streams.first()
    video_metadata.loc[i,"description"] = yt.description
    video_metadata.loc[i,"length_seconds"] = yt.length #in seconds
    video_id = video_url.split("v=")[1]

    try: # proceed with transcript extraction
        transcript_list = YouTubeTranscriptApi.list_transcripts(video_id)
        for transcript in transcript_list:
            video_metadata.loc[i,"language"] = transcript.language # export original language
            # different methods to export transcript files for non-English text
            if transcript.language == "English (auto-generated)": 
                english_transcript = transcript.fetch()
            else:
                english_transcript = transcript.translate('en').fetch()
        # export transcript to text files    
        f_name = 'data/side_events/youtube_transcripts/closed_captions/video_'+str(i)+'.txt'
        wordCount = 0
        with open(f_name, 'w') as the_file:
            for j in np.arange(len(english_transcript)):
                text_slice = english_transcript[j]["text"]
                start_time = english_transcript[j]["start"]
                # extract only the first hour of video
                if start_time<=3600 :
                    wds = len(text_slice.split())
                    wordCount += wds
                    the_file.write(text_slice+" ")
            the_file.close()
        video_metadata.loc[i,"extracted_wordCount"] = wordCount
        print("Video "+str(i)+" finished extraction: "+str(wordCount)+" words")
    except: # when the transcript is disabled
        video_metadata.loc[i,"language"] = "no transcript"
        print("Video "+str(i)+" doesn't have a transcript")

# export metadata
video_metadata.to_csv('data/side_events/youtube_transcripts/youtube_video_metadata.csv',
                      index_label="video_num")