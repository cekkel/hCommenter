import { useMutation, useQueryClient } from "@tanstack/react-query";
import { useState } from "react";
import {
  Button,
  Dialog,
  DialogTrigger,
  Form,
  Input,
  Label,
  OverlayArrow,
  Popover,
  TextArea,
  TextField,
} from "react-aria-components";
import {
  getCommentsByIdRepliesQueryKey,
  getCommentsConversationByConvoUrlQueryKey,
  getCommentsUserByUsernameQueryKey,
  postCommentsNewMutation,
} from "../../client/@tanstack/react-query.gen";

interface NewCommentProps {
  parentId?: number;
  convoUrl: string;
  triggerButtonText?: string;
}

export const NewComment = (props: NewCommentProps) => {
  const [isOpen, setIsOpen] = useState(false);
  const queryClient = useQueryClient();
  const createComment = useMutation({
    ...postCommentsNewMutation(),
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({
        queryKey: getCommentsConversationByConvoUrlQueryKey({
          path: { convoUrl: props.convoUrl },
        }),
      });
      queryClient.invalidateQueries({
        queryKey: getCommentsUserByUsernameQueryKey({
          path: { username: variables.body.author },
        }),
      });
      if (props.parentId) {
        queryClient.invalidateQueries({
          queryKey: getCommentsByIdRepliesQueryKey({
            path: { id: props.parentId },
          }),
        });
      }
      setIsOpen(false);
    },
  });

  const sendNewCommentRequest = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    const formData = new FormData(e.currentTarget);
    const content = formData.get("content") as string;
    const author = formData.get("author") as string;

    createComment.mutate({
      body: {
        message: content,
        parent: props.parentId,
        author: author,
        convoUrl: props.convoUrl,
      },
    });
  };

  const restartForm = () => {
    createComment.reset();
  };

  return (
    <DialogTrigger isOpen={isOpen} onOpenChange={setIsOpen}>
      <Button className="btn-primary">{props.triggerButtonText || "Add Comment"}</Button>
      <Popover
        className="w-[32rem]"
        onOpenChange={(isOpen: boolean) => {
          if (isOpen) createComment.reset();
        }}
      >
        <OverlayArrow className="placement-bottom:rotate-180 stroke-gray-400 fill-white">
          <svg width={12} height={12} viewBox="0 0 12 12">
            <path d="M0 0 L6 6 L12 0" />
          </svg>
        </OverlayArrow>
        <Dialog className="bg-white rounded-lg shadow-lg ring-1 ring-gray-400 p-6">
          <h2 className="text-2xl font-semibold mb-6 pb-2 border-b">New Comment</h2>
          {createComment.isPending && <div className="text-gray-500">Creating comment...</div>}

          {createComment.isError && (
            <div className="text-red-500 flex flex-col items-center gap-2">
              <p>An error occurred while trying to create your comment. Please try again.</p>
              <Button className="btn-secondary" onPress={restartForm}>
                Try again
              </Button>
            </div>
          )}

          {createComment.isSuccess && <div className="text-green-500">Comment created successfully!</div>}

          {createComment.isIdle && (
            <Form onSubmit={sendNewCommentRequest}>
              <TextField className="mb-4" isRequired>
                <Label className="text-sm font-medium mb-1">Name</Label>
                <Input name="author" className="input" placeholder="Your name" />
              </TextField>
              <TextField className="mb-4" isRequired>
                <Label className="text-sm font-medium mb-1">Comment</Label>
                <TextArea
                  name="content"
                  className="input min-h-[120px]"
                  placeholder="Write your comment here..."
                />
              </TextField>
              <div className="mt-6 flex justify-end gap-3">
                <Button onPress={() => setIsOpen(false)} className="btn-secondary">
                  Cancel
                </Button>
                <Button type="submit" className="btn-primary">
                  Submit
                </Button>
              </div>
            </Form>
          )}
        </Dialog>
      </Popover>
    </DialogTrigger>
  );
};
